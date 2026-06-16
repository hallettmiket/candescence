"""
Purpose: Run modern (torchvision) detector training in a background thread so the
         Streamlit UI stays responsive — live progress, a working Stop button, and
         freedom to navigate away while a job runs.
Author: Hallett Lab
Date: 2026-06-16
Input: The same arguments as candescence.detection.modern.train_detector.
Output: A TrainingJob holding a worker thread, a progress queue, and the result.

The worker thread only ever touches the job's own thread-safe primitives (a
``queue.Queue`` and a ``threading.Event``); it never calls Streamlit APIs or
mutates ``st.session_state``. The Streamlit script thread polls the queue (via
:meth:`TrainingJob.drain`) on each rerun and renders from the snapshot. This
keeps us clear of Streamlit's "missing ScriptRunContext" thread warnings.
"""

from __future__ import annotations

import queue
import threading
from dataclasses import dataclass, field
from typing import Dict, List, Optional

from candescence.core.logging_config import get_logger
from candescence.detection.modern import train_detector
from candescence.detection.modern.trainer import TrainResult

logger = get_logger("candescence.interface.training_jobs")


@dataclass
class TrainingJob:
    """A background training run and the snapshot the UI renders from."""

    events: "queue.Queue[Dict]" = field(default_factory=queue.Queue)
    stop_event: threading.Event = field(default_factory=threading.Event)
    thread: Optional[threading.Thread] = None
    result: Optional[TrainResult] = None
    error: Optional[str] = None
    done: bool = False
    # Snapshot updated from the script thread by ``drain()``.
    losses: List[float] = field(default_factory=list)
    last_event: Optional[Dict] = None

    def is_running(self) -> bool:
        """Whether the worker thread is still alive."""
        return self.thread is not None and self.thread.is_alive()

    def request_stop(self) -> None:
        """Ask the trainer to stop gracefully after the current step."""
        self.stop_event.set()

    @property
    def stop_requested(self) -> bool:
        return self.stop_event.is_set()

    def drain(self) -> None:
        """Move queued progress events into the render snapshot (script thread)."""
        while True:
            try:
                event = self.events.get_nowait()
            except queue.Empty:
                break
            self.last_event = event
            self.losses.append(float(event["loss"]))

    def fraction_done(self) -> float:
        """Best-effort completion fraction in ``[0, 1]`` from the last event."""
        event = self.last_event
        if not event or not event.get("total_steps") or not event.get("epochs"):
            return 0.0
        done = event["epoch"] * event["total_steps"] + event["step"] + 1
        total = event["epochs"] * event["total_steps"]
        return min(1.0, done / max(1, total))


def start_training_job(**train_kwargs) -> TrainingJob:
    """Launch ``train_detector`` in a daemon thread; return the live job handle.

    All keyword arguments are forwarded to
    :func:`candescence.detection.modern.train_detector`, except ``progress`` and
    ``should_stop``, which this helper wires to the job's queue and stop event.
    """
    job = TrainingJob()

    def _progress(event: Dict) -> None:
        job.events.put(event)

    def _should_stop() -> bool:
        return job.stop_event.is_set()

    def _run() -> None:
        try:
            job.result = train_detector(
                progress=_progress, should_stop=_should_stop, **train_kwargs
            )
        except Exception as exc:  # captured for the UI; logged for the server
            job.error = str(exc)
            logger.exception("background training failed")
        finally:
            job.done = True

    thread = threading.Thread(target=_run, name="candescence-train", daemon=True)
    job.thread = thread
    thread.start()
    logger.info("started background training job")
    return job
