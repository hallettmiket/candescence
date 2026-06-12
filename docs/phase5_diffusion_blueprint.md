# Phase 5 — Porting Blueprint: Jose's Latent-Diffusion Model (TLV `version-5`)

**Source (read-only):** `<candescence_master>/projects/tlv/38-diffusion/version-5/`
**Target:** lift `src/` into `src/candescence/tlv/architectures` + a diffusion trainer/inference module, reproduce `webapp/` tabs as Streamlit pages.
**Author:** blacksmith · **Date:** 2026-06-12

> **Headline correction up front:** This is *not* a Stable-Diffusion / HuggingFace-`diffusers`
> model and it is *not* a latent-space diffusion (no VAE-to-latent compression before diffusion).
> It is a **pure-PyTorch pixel-space conditional DDPM/DDIM** whose UNet is conditioned on a
> VAE-style **semantic code** via FiLM (AdaGroupNorm). "Latent" here = the 32-d semantic vector,
> not a spatial latent the diffusion runs in. Diffusion runs at full 128×128 resolution.

---

## 0. One-paragraph mental model

`TLVCondDiffVAE` = `SemanticEncoder` (image → 32-d Gaussian code `mu`/`logvar`) **+**
`DiffusionUNet` (noise predictor `eps_pred = unet(x_t, t, z_sem)`). The encoder's posterior
mean `mu` is used as the conditioning vector `z_sem`. At train time you noise the real image
`x0` to `x_t` and ask the UNet to predict the noise, while `z_sem` tells it *what to draw*.
At inference you (a) encode an image to get `z_sem`, then (b) run DDIM/DDPM sampling
conditioned on that `z_sem` to reconstruct/generate. Novel samples = sample `z_sem ~ N(0,I)`
then sample. Interpolation = lerp two `mu`s and sample each. **The 32-d code is the embedding.**

---

## 1. MODEL ARCHITECTURE

All in `version-5/src/models/`. Every `nn.Module`:

### 1.1 `SemanticEncoder` — `src/models/encoder.py:17`
- **Constructor (`:26`):** `__init__(self, in_ch: int, latent_dim: int, img_size: int, ch_schedule: List[int] = None, enc_norm: bool = True)`
  - `ch_schedule` default `[64, 128, 256, 512]`.
- **forward (`:63`):** `forward(self, x0: Tensor) -> Tuple[z, mu, logvar]`
  - input `x0` `(B, C, 128, 128)`; output `z, mu, logvar` each `(B, latent_dim)` = `(B, 32)`.
  - 4 stages, each `Conv→GN→ReLU→Conv→GN→ReLU→Conv(stride2)→GN→ReLU` (128→64→32→16→8).
  - `flatten_dim = ch_schedule[-1] * (img_size // 2**4)**2 = 512 * 8*8 = 32768`; two `Linear(32768→32)` heads (`fc_mu`, `fc_logvar`).
  - Reparam trick only in `.training`; eval returns `z = mu`.
- **helper `gn_groups(C)` (`:9`):** GroupNorm group chooser (≤32 dividing C). Imported by `unet.py`.

### 1.2 `DiffusionUNet` — `src/models/unet.py:153`
- **Constructor (`:162`):** `__init__(self, in_ch=3, base_chs=[64,128,256,512], latent_dim=32, time_dim=256, sin_dim=128, T=500, num_heads=4)`
- **forward (`:250`):** `forward(self, x_t: Tensor, t: Tensor, z_sem: Tensor) -> eps_pred`
  - `x_t` `(B,C,128,128)`, `t` `(B,)` long, `z_sem` `(B,32)` → `eps_pred` `(B,C,128,128)`.
  - Down: 4 stages `[ResBlock, ResBlock, SelfAttention-or-Identity]`, attention only at 16×16/8×8. Strided-conv downsample between stages (128→64→32→16).
  - Mid: `ResBlock → SelfAttention → ResBlock` at 16×16.
  - Up: 4 stages, nearest-upsample + 3×3 conv, **skip concat** (`in_ch = ch+ch`), attention at 16×16.
  - Head: `GroupNorm(32) → SiLU → Conv2d(base_chs[0]→in_ch, 3)`.

### 1.3 Building blocks (also `src/models/unet.py`)
- **`AdaGroupNorm` (`:37`)** — *the conditioning mechanism.* `__init__(num_channels, latent_dim)`; `forward(h, z_sem)`. `affine=False` GroupNorm, then `Linear(latent_dim → 2*C)` (zero-init) produces per-channel `gamma,beta`: `h_norm * (1+gamma) + beta`. **This is FiLM.**
- **`TimeEmbedding` (`:61`)** — `__init__(sin_dim=128, out_dim=256, T=100)`; `forward(t) -> (B,256)`. Sinusoidal embed (`sinusoidal_timestep_embedding`, `:17`, normalizes `t/(T-1)`) → MLP.
- **`ResBlock` (`:79`)** — `__init__(in_ch, out_ch, time_dim=256, latent_dim=32)`; `forward(x, t_emb, z_sem)`. Two conv + AdaGroupNorm; time injected additively via `Linear(time_dim→out_ch)`.
- **`SelfAttention` (`:111`)** — `__init__(channels, num_heads=4)`; `forward(h)`. Multi-head, uses `F.scaled_dot_product_attention` (Flash on A100). Residual.

### 1.4 Top-level `TLVCondDiffVAE` — `src/models/vae.py:13`
- **Constructor (`:22`):** `__init__(self, config: DictConfig)` — reads `config.data.input_channels`, `config.data.img_size`, `config.model.encoder.*`, `config.model.unet.*`, `config.diffusion.t_steps`. Builds `self.encoder` + `self.unet`.
- **`encode` (`:44`):** `encode(self, x0) -> (z, mu, logvar, z_sem)` where `z_sem = mu`.
- **`predict_eps` (`:51`):** `predict_eps(self, x_t, t, z_sem) -> eps_pred` (just calls `self.unet`).
- *No `forward()`* — the trainer/inference call `encode` + `predict_eps` + schedule helpers explicitly.

### 1.5 Noise scheduler — `src/diffusion/schedule.py` (functional, not a Module)
- `build_linear_schedule(T, beta_start, beta_end, device) -> (betas, alphas, alpha_bars)` (`:11`)
- `build_cosine_schedule(T, device, s=0.008)` (`:25`)
- `build_schedule(cfg, device)` (`:43`) — dispatches on `cfg.diffusion.schedule_type`.
- `q_sample(x0, t, alpha_bars, eps=None) -> (x_t, eps)` (`:55`) — forward diffusion.
- `predict_x0_from_eps(x_t, t, eps_pred, alpha_bars)` (`:69`).

### 1.6 Key numbers (from `configs/` defaults)
| Property | Value | Source |
|---|---|---|
| Input size | **128×128** | `configs/data/default.yaml` |
| Channels | **3 (RGB)** by default; on-disk ckpts are **1 (grayscale)** | `data.input_channels` |
| Latent / semantic dim | **32** | `model/default.yaml` |
| Diffusion timesteps `T` | **1000** (config) / **500** (DEBUG ckpt) | `diffusion/default.yaml` |
| Beta schedule | **linear**, `beta_start=1e-4`, `beta_end=2e-2` | `diffusion/default.yaml` |
| DDIM steps | **50** (`ddim_k`) | `diffusion/default.yaml` |
| UNet channels | `[64,128,256,512]`, time_dim 256, sin 128, 4 heads | `model/default.yaml` |

---

## 2. CONDITIONING

- **What it conditions on:** the **learned 32-d semantic code `z_sem = mu`** from `SemanticEncoder`. **Nothing else.** There is **no hue/HSV/media/day conditioning anywhere in `version-5/src`.** (Grep confirms `hue`/`saturation`/`media` appear only in dataset *augmentation* and webapp dataset filtering, never as model inputs.)
- **How:** **FiLM via `AdaGroupNorm`** (`unet.py:37`). Every `ResBlock` normalizes with `affine=False` GroupNorm and applies per-channel `(1+gamma)·h + beta`, where `gamma,beta = Linear(z_sem)`. Zero-init means conditioning starts as identity. Time is conditioned **separately** (additive projection), not through FiLM.
- **Not used:** cross-attention conditioning, concatenation conditioning. `SelfAttention` is spatial self-attn only (no context tokens).
- **Consistency note for the target repo (HSV-FiLM VAEs):** the FiLM mechanism is *identical in spirit* to your VAE H/S/V FiLM. To make this model HSV-conditioned, you'd concatenate `[z_sem, hsv_vec]` (or add a parallel FiLM generator) into the `AdaGroupNorm.to_params` input — a clean ~10-line change since the FiLM plumbing already threads `z_sem` to every block. **Recommended:** keep `z_sem` as the semantic channel and *add* an HSV FiLM input rather than replacing it.

---

## 3. TRAINING

- **Entrypoint loop:** `train(...)` in `src/training/trainer.py:159`
  `train(model, train_loader, val_loader, val_ds, cfg, device, resume_checkpoint=None)`.
  (No `if __name__ == "__main__"` runner in version-5/src; Hydra `config.yaml` drives it — assume a `run.py`/notebook constructs `model`, dataloaders, and calls `train`.)
- **Data loading:** `build_dataloaders(cfg)` in `src/data/dataset.py:128` → `(train_loader, val_loader, train_ds, val_ds)`. 90/10 split, seed 42, `batch_size=128`, `num_workers=8`. Train batch yields `(x0, x_aug)`; val yields `(x0, None)` via `_val_collate`.
- **Loss terms** (`trainer.py:342–369`), weights from `configs/training/default.yaml`:
  - `L_diff` = MSE(`eps_pred`, `eps`)  · `w_diff=1.5`
  - `L_sem` = `w_sem_mse·MSE(x0_pred_1step, x0)` + `w_sem_lpips·LPIPS(x0_pred_1step, x0)` · `w_sem_mse=0.75`, `w_sem_lpips=0.5`
  - `L_edge` = MSE(Sobel edges) · `w_edge=0.1` (`losses.py:get_edge_features`)
  - `L_drift` = MSE(`mu`, `mu_aug`) — augmentation invariance · `w_drift=0.5`
  - `L_kl` = KL(q‖N(0,I)) (`losses.py:42`) · `w_kl=1e-3` with **KL annealing** over `kl_anneal_epochs=30` from `kl_anneal_start=1e-4`.
  - `x0_pred_1step = predict_x0_from_eps(...)` is the single-step x0 estimate used for the perceptual/edge/sem losses.
- **Optimizer:** `torch.optim.Adam(lr=1e-4, betas=(0.9,0.999), eps=1e-8, weight_decay=0)` (`:196`).
- **Tricks baked in:** `torch.compile(mode="reduce-overhead")` (`:181`), BF16 autocast (`:342`), `GradScaler`, optional cosine/plateau/exp LR scheduler (`_build_scheduler` `:125`), optional **EMA** (`EMAModel` `:96`, off by default), optional grad-clip, optional early-stopping.
- **Diagnostics:** `save_reconstruction_grid(...)` (`:39`) writes Original|DDIM|DDPM grids; WandB optional.
- **Hard third-party deps inside the loop:** `lpips` (downloads VGG weights at first run), `wandb`, `tqdm`.

### Checkpoint format (`src/training/checkpoint.py:11` `save_checkpoint`)
`torch.save` dict with keys:
`model` (state_dict), `optim`, `epoch`, `logs`, `config` (`OmegaConf.to_container`),
and optionally `scaler`, `global_step`, `ema`, `scheduler`, `best_val_loss`, `epochs_without_improvement`.
Best model saved as `saves/best.pt`; periodic `saves/ckpt_epoch_NNNN.pt`.
**`load_checkpoint(model, path, optimizer=None, map_location=None)` (`:49`).**
**Compile-prefix gotcha:** keys may be prefixed `_orig_mod.` (from `torch.compile`); both loaders strip it (`trainer.py:257`, `model_service.py:112`).

---

## 4. INFERENCE / GENERATION  *(highest priority)*

Two layers: **schedule primitives** (`src/diffusion/schedule.py`) and the **webapp service wrappers**
(`webapp/backend/services/inference_service.py`) which add model/dataset caching, base64, metrics.

### 4.1 Schedule primitives (the math — port these verbatim)
- **Generate novel from noise (DDIM):** `generate_ddim(model, z_sem, alpha_bars, cfg, K=None, shape=None) -> x0` in `[-1,1]` (`schedule.py:85`). `K` defaults to `cfg.diffusion.ddim_k=50`.
- **Generate (DDPM):** `generate_ddpm(model, z_sem, betas, alphas, alpha_bars, cfg, shape=None)` (`:118`).
- **Reconstruct (DDIM):** `reconstruct_ddim(model, z_sem, x_noisy, alpha_bars, cfg) -> [0,1]` (`:154`).
- **Reconstruct (DDPM):** `reconstruct_ddpm(model, z_sem, x_noisy, betas, alphas, alpha_bars, cfg) -> [0,1]` (`:178`).
- For reconstruction you first noise the input: `x_noisy,_ = q_sample(x0, t=T-1, alpha_bars)`.

### 4.2 Service wrappers (the app-facing API — copy the signatures)
File: `webapp/backend/services/inference_service.py`. All assume a globally-loaded model
(`model_service.get_current()` → `LoadedModel`).
- **(a) Reconstruct an input:** `reconstruct(indices, method="ddim", seed=None, ddim_steps=None) -> {originals[b64], reconstructions[b64], metrics[{psnr,ssim}]}` (`:73`). Encodes `x0→z_sem`, noises to `T-1`, runs `reconstruct_ddim/ddpm`.
- **(b) Generate novel from noise:** `generate(n_images=4, seed=None, latent_vectors=None, method="ddim", ddim_steps=None, from_data=False) -> {images[b64], (originals[b64])}` (`:137`). If `latent_vectors` given → use them; if `from_data` → encode random dataset images; else `z_sem = randn(n, latent_dim)`.
- **(c) Interpolate in latent space:** `interpolate(idx_a, idx_b, n_steps=8, method="ddim", seed=None) -> List[b64]` (`:241`). Encodes both → linear lerp of `mu` → sample each.
- **(d) Extract latent embeddings:** `encode(indices) -> (mus[list], logvars[list])` (`:56`). **Encodes one-by-one** (comment warns batch encoding differs — match this in the port). Sprite/UMAP path: `sprite_service.generate(...)` (`sprite_service.py:20`) encodes N samples and PCA/UMAP-projects `mu`.
- **Helpers:** `_tensor_to_b64` (`:25`), `_psnr` (`:37`), `_ssim_simple` (`:44`), `find_neighbors` (`:192`) for latent-space neighbor lookup.

### 4.3 Model loading (`webapp/backend/services/model_service.py`)
- **`load_model(path, device=None, legacy=False) -> LoadedModel`** (`:76`). Builds `TLVCondDiffVAE(cfg)`, **prefers EMA shadow weights** if present (`:107`), strips `_orig_mod.`, builds schedule buffers. Caches globally (`_current`).
- **`LoadedModel`** dataclass (`:22`): `model, cfg, epoch, logs, path, device, betas, alphas, alpha_bars`.
- **`_convert_legacy_config(raw)` (`:48`):** remaps the **flat notebook config** (`ENCODER/UNET/INPUT_CHANNELS/...` or top-level `encoder/unet/model`) into the nested `model.encoder/model.unet` + `data`/`diffusion` layout. **You MUST port this — the on-disk checkpoints use the legacy layout (see §8).**

---

## 5. DEPENDENCIES

**Pure PyTorch — NO HuggingFace `diffusers`/`transformers`.** Schedule + UNet + sampling are hand-written.

`version-5/requirements.txt`: `torch>=2.0.0`, `torchvision>=0.15.0`, `hydra-core>=1.3.0`,
`omegaconf>=2.3.0`, `lpips>=0.1.4`, `numpy`, `scipy`, `scikit-learn`, `matplotlib`, `Pillow`, `tqdm`, `wandb`.

`webapp/requirements.txt`: `gradio>=4.0`, `fastapi`, `uvicorn[standard]`, `plotly`, `pydantic>=2.0`,
`httpx`, `Pillow`, `numpy`, `torch`, `torchvision`, `omegaconf`, `python-jose[cryptography]`, `bcrypt`.

**Flags for integration into a torch≥2.0 repo:**
- `omegaconf` is load-bearing — the model constructor takes a `DictConfig` and checkpoints store an OmegaConf dump. Either keep `omegaconf` or refactor the constructor to a plain dataclass/dict (small change).
- `hydra-core` only drives the CLI/config composition; **not needed for inference** — drop it in the port.
- `lpips` only needed for **training** (pulls VGG weights). Not needed for inference. Make it a lazy/optional import.
- `wandb` training-only and already wrapped in try/except.
- `torch.compile` is used in the trainer; safe on torch≥2.0 but disable for small/CPU runs.
- GUI port to Streamlit drops `gradio`, `fastapi`, `uvicorn`, `python-jose`, `bcrypt` entirely.

---

## 6. DATA FORMAT

- **Class:** `CandidaColonyDataset` (`src/data/dataset.py:14`). Recursively globs `*.png/*.jpg/*.jpeg/*.bmp/*.tiff` under `root`.
- **Pipeline (`base_tf`, `:76`):** `Resize((img_size,img_size)) → ToTensor → Normalize(mean=[0.5]*C, std=[0.5]*C)` ⇒ tensors in **`[-1, 1]`**. Grayscale via `.convert("L")` when `input_channels==1`, else `.convert("RGB")`.
- **Train aug:** geometric (h/v flip, ±180° rot), color jitter (contrast/sat/hue), brightness factor. Train item = `(x0, x_aug)`; val item = `(x0, None)`.
- **Default data root:** `<refined>/candescence_master/projects/tlv/0.2-images_cut/all-final/` — **same TLV colony images as the VAEs.**
- **Confirmed conventions:** images are **`.bmp`**, filename pattern **`P{plate}_{media}_{day}_{rep}-r{row}-c{col}.bmp`** (e.g. `P11_control_day2_1-r1-c10.bmp`). **50,880 images** in that folder. Native size is the crop size; dataset resizes to 128×128. **Yes — it reuses the same colony images and the same `P{plate}_{media}_{day}...` convention the TLV VAEs use.**

---

## 7. THE GUI (`version-5/webapp/`)

Architecture: **Gradio Blocks frontend** (`webapp/frontend/`) + **FastAPI backend** (`webapp/backend/`)
with SQLite (`candescence.db`), JWT auth (`python-jose`/`bcrypt`), and a service layer
(`backend/services/`). Tabs assembled in `frontend/app.py:create_gradio_app` (`:19`).

| Tab (`frontend/tabs/`) | Backend service fn | What it shows |
|---|---|---|
| `overview.py` | `model_service.current_info`, `list_checkpoints` | Loaded-model info, checkpoint picker/loader. |
| `reconstruction.py` | `inference_service.reconstruct` | Original vs DDIM/DDPM recon + PSNR/SSIM. |
| `generation.py` | `inference_service.generate` | Novel samples from noise / from-data / from latent vectors. |
| `interpolation.py` | `inference_service.interpolate` | Morph strip between two images (lerp of `mu`). |
| `latent_space.py` | `inference_service.encode`, `find_neighbors`, `sprite_service` | 2-D scatter of `mu` (PCA/UMAP), nearest-neighbor lookup. |
| `sprites.py` | `sprite_service.generate` | Sprite map of N encoded images laid out by projection/cluster; PNG download. |
| `dataset_browser.py` | `dataset_service.browse`/`get_image`/`stats` | Paginated thumbnails, per-image + augmented view; media-filtering. |
| `training.py` | `services/training_runner.py`, `queue_manager.py` | Launch/queue training runs, live status. |
| `experiments.py` | `backend/routers/experiments.py` + DB | Saved experiment records. |
| `logs.py` | reads `diagnostics/loss_log.json` | Loss curves / run logs. |

**Reproduce-as-Streamlit priority:** `reconstruction`, `generation`, `interpolation`, `latent_space`/`sprites`,
`dataset_browser`. Skip/defer `training`, `experiments`, `logs`, auth, SQLite — they're operational scaffolding, not core science.

---

## 8. PRETRAINED CHECKPOINTS (on disk)

Under `<refined>/candescence_master/projects/tlv/38-diffusion/`:

| Path | Size | Notes |
|---|---|---|
| `DEBUG/models/ckpt_epoch_0075.pt` | **657 MB** | **Loadable.** 310 tensors, **no** `_orig_mod.` prefix, **no** schedule buffers. Legacy flat config: `MODEL_VERSION=BW-1.0.0, MODEL_TYPE=L, INPUT_CHANNELS=1 (grayscale), LATENT_DIM=32, T_STEPS=500`. Architecture matches `TLVCondDiffVAE` exactly (`encoder.conv.0.weight=(64,1,3,3)`, `unet.out_conv.weight=(1,64,3,3)`). **Needs `legacy=True` config conversion + input_channels=1, T_steps=500.** |
| `diffusion_vae_test/models/diffusion_epoch_0001.pt … _0014.pt` | **751 MB each** (14 files) | **Risky.** 313 tensors, **also 1-channel** (`encoder.conv.0.weight=(64,1,3,3)`), but state_dict embeds extra buffers `betas/alphas/alpha_bars` AND config is **incomplete** (`{diffusion_loss_weights, lr, betas, eps}` only — no model/data/diffusion structure). Loading needs a hand-built config + `strict=False` (the 3 schedule buffers won't match). Use only if DEBUG proves insufficient. `_0014` is the latest epoch. |

There are **no RGB checkpoints on disk** despite the default config being RGB — all trained weights are **grayscale (1-channel)**. The newest/cleanest loadable artifact is **`DEBUG/models/ckpt_epoch_0075.pt`** (BW-1.0.0, T=500). Permissions: DEBUG ckpt is group-readable; `diffusion_vae_test` ckpts are `rwxrwx---` owned by `wzhan564` (verify read access).

---

## 9. RECOMMENDED PORT PLAN

### 9.1 Files/classes to copy → `src/candescence/tlv/architectures/` (+ a diffusion module)
Copy these **verbatim** (pure, self-contained, no app deps):
1. `src/models/encoder.py` → `architectures/diffusion_encoder.py` (`SemanticEncoder`, `gn_groups`).
2. `src/models/unet.py` → `architectures/diffusion_unet.py` (`DiffusionUNet` + `AdaGroupNorm`, `TimeEmbedding`, `ResBlock`, `SelfAttention`, `sinusoidal_timestep_embedding`).
3. `src/models/vae.py` → `architectures/cond_diff_vae.py` (`TLVCondDiffVAE`).
4. `src/diffusion/schedule.py` → `architectures/diffusion_schedule.py` (all schedule + sampling fns).
5. `src/losses/losses.py` → a `diffusion_losses.py` (only if you'll retrain; inference doesn't need it).

New thin modules to write (adapt, don't copy wholesale):
6. `diffusion_inference.py` — port `encode/reconstruct/generate/interpolate` from `inference_service.py`, **stripping** the `model_service`/`dataset_service` globals; take an explicit `LoadedModel`-like object as an argument.
7. `diffusion_model_loader.py` — port `LoadedModel` + `load_model` + **`_convert_legacy_config`** from `model_service.py`. **This is mandatory** to load the on-disk checkpoints.
8. (optional) `diffusion_trainer.py` — port `trainer.py` only if retraining.

### 9.2 What to rename/adapt
- Replace the `DictConfig` constructor of `TLVCondDiffVAE` with a small frozen dataclass `DiffVAEConfig` (img_size, input_channels, latent_dim, unet channels, t_steps, beta range, schedule_type, ddim_k) **OR** keep `omegaconf` and feed it an `OmegaConf.create(...)`. Recommend dataclass to drop the omegaconf+hydra dep for inference.
- Set defaults to **grayscale**: `input_channels=1`, `T_steps=500`, to match the loadable DEBUG checkpoint (override per-checkpoint from its stored config).
- Keep the `_orig_mod.` strip and the EMA-preference logic in the loader.

### 9.3 Minimal dependency additions
For **inference-only** in your torch≥2.0 app: **just `torch`, `torchvision`, `numpy`, `Pillow`** (all already present). Add `omegaconf` **only if** you keep the DictConfig path. **Do not** add `diffusers`, `hydra`, `lpips` (training-only), `wandb`, `gradio`, `fastapi`.

### 9.4 Smallest path to "load a pretrained model and generate/reconstruct in the app"
1. Copy files 1–4 above into `architectures/`.
2. Write `diffusion_model_loader.load_model(path, device, legacy=True)` (port `_convert_legacy_config`).
3. Point it at `…/38-diffusion/DEBUG/models/ckpt_epoch_0075.pt` with `input_channels=1, T_steps=500`.
4. Build the schedule via `build_schedule(cfg, device)` → `(betas, alphas, alpha_bars)`.
5. Reconstruct: `q_sample(x0, T-1) → reconstruct_ddim(model, z_sem, x_noisy, alpha_bars, cfg)`.
6. Generate: `z_sem = randn(n,32); generate_ddim(model, z_sem, alpha_bars, cfg, K=50)`.
7. Wrap in two Streamlit pages (reconstruction, generation) using the b64 helpers.

### 9.5 TOP 3 RISKS
1. **Checkpoint/config mismatch (highest).** The only clean on-disk weights use a **legacy flat config** and are **grayscale**, while `version-5` defaults are **RGB with nested config**. Without porting `_convert_legacy_config` and forcing `input_channels=1, T_steps=500`, `load_state_dict` will shape-mismatch. The `diffusion_vae_test` ckpts additionally embed schedule buffers + a stub config → need `strict=False` and a manual config.
2. **"Latent diffusion" misnomer / cost.** Diffusion runs at **full 128×128**, not in a compressed latent. DDPM sampling = `T` (500–1000) UNet passes per image; even DDIM is 50 passes. On CPU this is slow — the app must default to DDIM with small `K` and ideally GPU. Don't assume `diffusers`-style fast latent sampling.
3. **`z_sem` is the only conditioning — no HSV/media.** If the goal is HSV-FiLM consistency with your VAEs, this model does **not** provide it out of the box; you'd extend `AdaGroupNorm`'s input to accept an HSV vector. Also: encoding is done **one-image-at-a-time** in the source (batch encoding gives different `mu`); preserve that to reproduce sprite/latent-space results.
