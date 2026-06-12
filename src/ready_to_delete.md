# Ready to Delete

This file tracks data and files in `<refined>/candescence_new/` that can be safely deleted.

## Format

```
- [date] path/to/file - reason for deletion
```

## Files Ready for Deletion

- [2026-02-05] `obsolete/deprecated_registries/production_registry.py` - Replaced by `core/model_zoo.py` (ModelZoo)
- [2026-02-05] `obsolete/deprecated_registries/core_model_registry.py` - Replaced by `core/model_zoo.py` (ModelZoo)
- [2026-02-05] `obsolete/deprecated_registries/interface_model_registry.py` - Replaced by `core/model_zoo.py` (ModelZoo)
- [2026-02-05] `obsolete/deprecated_registries/interface_dataset_registry.py` - Replaced by `core/dataset_zoo.py` (DatasetZoo)
- [2026-02-05] `<refined>/candescence_new/production_models.json` - Superseded by `zoo/registry.json`
- [2026-02-05] `<refined>/candescence_new/production_datasets.json` - Superseded by `zoo/datasets.json`
- [2026-02-05] `<refined>/candescence_new/model_registry.json` - Superseded by `zoo/registry.json`
- [2026-02-05] `<refined>/candescence_new/dataset_registry.json` - Superseded by `zoo/datasets.json`
- [2026-04-14] `obsolete/strategy_15_cond_tendrils/cond_tendrils.py` - Replaced by per-variable FiLM Strategy 15 (multi_cond_tendril_VAE on outer VAE; inner tendrils reverted to plain Tendrils like Strategy 14)
- [2026-04-14] `obsolete/strategy_15_cond_tendrils/test_strategy_15_old.py` - Tests for retired CondTendril* classes; superseded by new tests/tlv/test_strategy_15.py
- [2026-06-12] `<refined>/candescence_new/_uploads/63c8b92cd5a5/` - Empty staging dir created by a manual AppTest of the image picker before the lazy-create fix; safe to remove (protected_paths hook blocks automated deletion).

---

## Guidelines

1. Before deleting any file, add it to this list with:
   - Date marked for deletion
   - Full path relative to refined directory
   - Reason why it can be deleted

2. Files in `obsolete/` folders should be listed here

3. Wait at least 1 week after marking before actual deletion

4. After deletion, move the entry to "Deleted Files" section below

## Deleted Files

*No files deleted yet.*
