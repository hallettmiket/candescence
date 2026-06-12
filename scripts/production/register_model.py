"""
Purpose: Register a trained model in the unified model zoo via command line
Author: Hallett Lab
Date: 2026-02-05

This script provides a CLI interface for registering trained models
in the ModelZoo.

Usage:
    # Register a model by experiment/run name:
    python register_model.py \
        --experiment tutorial_tendril_vae \
        --run test_run_v1 \
        --name "My Production Model" \
        --version 1.0.0 \
        --description "Trained on day 2 images"

    # Register by direct path:
    python register_model.py \
        --path /path/to/refined/my_exp/run/models/model.pth \
        --id my_model_v1 \
        --name "My Model" \
        --version 1.0.0

    # List registered models:
    python register_model.py --list

    # Show model info:
    python register_model.py --info my_model_id

    # Unregister a model:
    python register_model.py --unregister my_model_id

Environment:
    conda activate candescence_new
"""

import argparse
import json
import sys
from pathlib import Path

# Add src to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent / "src"))

from candescence.core.model_zoo import ModelZoo


def list_models(zoo: ModelZoo) -> None:
    """List all registered models."""
    models = zoo.list_models()

    if not models:
        print("No models registered in zoo.")
        return

    print(f"\nRegistered Models ({len(models)}):")
    print("=" * 60)

    for entry in models:
        print(f"\n  {entry.id}")
        print(f"    Name: {entry.name}")
        print(f"    Project: {entry.project}")
        print(f"    Type: {entry.model_type}")
        print(f"    Version: {entry.version}")
        print(f"    Architecture: {entry.architecture}")
        print(f"    Path: {entry.path}")
        status = "exists" if entry.exists() else "NOT FOUND"
        print(f"    Checkpoint: {entry.checkpoint} ({status})")
        if entry.description:
            print(f"    Description: {entry.description[:60]}...")


def show_model_info(zoo: ModelZoo, model_id: str) -> None:
    """Show detailed information for a specific model."""
    entry = zoo.get(model_id)
    if entry is None:
        print(f"ERROR: Model '{model_id}' not found in zoo")
        sys.exit(1)

    print(f"\nModel: {model_id}")
    print("=" * 60)
    from dataclasses import asdict
    print(json.dumps(asdict(entry), indent=2, default=str))


def register_model(
    zoo: ModelZoo,
    model_path: Path,
    model_id: str,
    name: str,
    version: str,
    description: str,
    project: str = "tlv",
) -> None:
    """Register a model in the zoo."""
    if not model_path.exists():
        print(f"ERROR: Model file not found: {model_path}")
        sys.exit(1)

    try:
        zoo.register(
            model_id=model_id,
            name=name,
            project=project,
            model_type="production",
            version=version,
            architecture="tendril_vae",
            checkpoint=model_path.name,
            config_file="args.json",
            path=model_path.parent,
            description=description,
            tags=["tendril", "vae"],
        )
        print(f"\nSuccessfully registered model in zoo!")
        print(f"  ID: {model_id}")
        print(f"  Name: {name}")
        print(f"  Version: {version}")
        print(f"  Path: {model_path}")
    except Exception as e:
        print(f"ERROR: {e}")
        sys.exit(1)


def unregister_model(zoo: ModelZoo, model_id: str) -> None:
    """Remove a model from the zoo (does not delete files)."""
    if zoo.remove(model_id):
        print(f"Successfully removed model from zoo: {model_id}")
        print("Note: Model files were NOT deleted, only removed from registry.")
    else:
        print(f"ERROR: Model '{model_id}' not found in zoo")
        sys.exit(1)


def main():
    parser = argparse.ArgumentParser(
        description="Register trained models in the unified model zoo",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Register by experiment name:
  python register_model.py --experiment my_exp --run v1 --name "My Model" --version 1.0.0

  # List all models:
  python register_model.py --list

  # Show model details:
  python register_model.py --info my_model_id
        """
    )

    # Actions
    parser.add_argument("--list", action="store_true", help="List all registered models")
    parser.add_argument("--info", metavar="MODEL_ID", help="Show info for a specific model")
    parser.add_argument("--unregister", metavar="MODEL_ID", help="Remove a model from zoo")

    # Registration by experiment/run
    parser.add_argument("--experiment", help="Experiment name (folder under refined/)")
    parser.add_argument("--run", help="Run/save name (subfolder under experiment)")

    # Registration by direct path
    parser.add_argument("--path", type=Path, help="Direct path to model.pth file")
    parser.add_argument("--id", dest="model_id", help="Model ID (required with --path)")

    # Model metadata
    parser.add_argument("--name", help="Human-readable model name")
    parser.add_argument("--version", default="1.0.0", help="Version string (default: 1.0.0)")
    parser.add_argument("--description", default="", help="Model description")
    parser.add_argument("--project", default="tlv", help="Project name (default: tlv)")

    args = parser.parse_args()

    # Initialize zoo
    zoo = ModelZoo()

    # Handle actions
    if args.list:
        list_models(zoo)
        return 0

    if args.info:
        show_model_info(zoo, args.info)
        return 0

    if args.unregister:
        unregister_model(zoo, args.unregister)
        return 0

    # Handle registration
    if args.path:
        if not args.model_id:
            print("ERROR: --id is required when using --path")
            return 1
        if not args.name:
            print("ERROR: --name is required for registration")
            return 1

        model_path = args.path
        model_id = args.model_id

    elif args.experiment and args.run:
        if not args.name:
            print("ERROR: --name is required for registration")
            return 1

        from candescence.core.settings import get_settings

        base_path = get_settings().refined_path
        model_path = base_path / args.experiment / args.run / "models" / "model.pth"
        model_id = f"{args.experiment}_{args.run}".replace("/", "_").replace("-", "_")

    else:
        parser.print_help()
        print("\nERROR: Specify either --experiment/--run or --path for registration")
        return 1

    register_model(
        zoo=zoo,
        model_path=model_path,
        model_id=model_id,
        name=args.name,
        version=args.version,
        description=args.description,
        project=args.project,
    )

    return 0


if __name__ == "__main__":
    sys.exit(main())
