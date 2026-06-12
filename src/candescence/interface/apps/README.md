# Candescence Interactive Apps

Standalone Streamlit applications for interactive exploration of Candescence models.

## Latent Space Explorer

Interactive tool for exploring TLV model latent spaces.

### Features

- **Click on data points**: View the corresponding real image and metadata
- **Click on empty space**: Decode from that 2D position using k-NN weighted interpolation
- **Select two points**: View both images and generate an interpolation transition
- **Multiple projections**: UMAP, t-SNE, or PCA
- **Color by attributes**: Visualize metadata like hue, saturation, colony size

### Installation

Install required dependencies:

```bash
pip install streamlit plotly streamlit-plotly-events
```

For UMAP support (optional):
```bash
pip install umap-learn
```

### Usage

#### From command line:

```bash
cd <repo>
streamlit run src/candescence/interface/apps/latent_explorer_app.py
```

This will open a browser window with the interactive explorer.

#### From Python:

```python
from candescence.interface.apps import latent_explorer_main
latent_explorer_main()
```

### Data Loading Options

1. **From Model + Images** (recommended)
   - Load a trained candescence_new model directly
   - Computes embeddings on-the-fly from raw images
   - Full decoding support for interpolation and empty-space exploration
   - Available models:
     - `tutorial_tendril_vae/test_run_v1` - Tutorial training run
     - `0_37_tendrils_reproduction/manuscript_v1` - 37 tendrils reproduction
   - Images loaded from: `<raw>/candescence_new/tlv_images/all-final/`

2. **From Inference Object** (legacy)
   - Load a saved `inference_obj*.pkl` file from candescence_master
   - Note: May have dependency issues (skbio, etc.) due to pickle format
   - Path example: `<legacy-refined>/candescence_master/projects/tlv/*/analyses/inference_obj*.pkl`

3. **From NPZ + Images**
   - Load pre-computed embeddings from `.npz` file
   - Load images from `.npy` or `.npz` file
   - Optionally load metadata from `.csv` file
   - Use this if you've exported embeddings separately

4. **Demo Data**
   - Generate synthetic data for testing the interface
   - Useful for verifying the installation works

### Workflow

1. **Load data**: Use the sidebar to select your data source
2. **Configure visualization**: Choose projection method (PCA/t-SNE/UMAP) and color attribute
3. **Explore**:
   - Click on any point to see its image
   - Click on empty space to see a decoded image from that position
   - Click a second point to enable interpolation
4. **Interpolate**: Adjust the number of steps and generate the interpolation filmstrip
5. **Clear**: Use "Clear Selection" to start fresh

### Without streamlit-plotly-events

If `streamlit-plotly-events` is not installed, the app falls back to manual selection:
- Enter point indices manually
- Enter X/Y coordinates for empty space points

### Technical Notes

- **k-NN Decoding**: When clicking on empty space, the app estimates the latent vector using inverse-distance weighted average of the k=5 nearest neighbors in 2D space, then decodes from that position.

- **Skip Connections**: For U-Net style architectures, the app sets skip connections by encoding a reference image (nearest neighbor) before decoding.

- **Session State**: The app maintains state across interactions. Use "Clear Selection" or reload the page to reset.

### Troubleshooting

**"No module named 'skbio'" or "No module named 'inference'" when loading inference objects**
- This happens with old pickle files from candescence_master
- **Recommended solution**: Use "From Model + Images" instead
- This loads models from candescence_new directly without pickle dependency issues

**"UMAP not available, falling back to t-SNE"**
- Install umap-learn: `pip install umap-learn`

**Click events not working**
- The app uses manual index selection (hover to see index, then enter)
- This is the most reliable method across Streamlit versions

**"No inference objects found"**
- Verify the path contains `.pkl` files
- Check file permissions
- Or switch to "From Model + Images" loading

**Memory issues with large datasets**
- Use PCA instead of t-SNE/UMAP (faster, less memory)
- Reduce "Max samples to load" when using "From Model + Images"
- Typical recommendation: 500-1000 samples
