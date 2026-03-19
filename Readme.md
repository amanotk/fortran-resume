# Fortran演習レジュメ

## 使い方

```
$ uv sync
$ uv run make html
```
で_build/htmlにhtmlが作成される．ローカルチェック用．

GitHub Pages への公開はこのリポジトリの GitHub Actions から行う．

```
$ uv run make pubhtml
```
でpublicにhtmlが作成される．必要な場合の手動出力用．


依存パッケージのバージョンは `pyproject.toml` と `uv.lock` で固定する．


## メモ

- 古い `sphinx_rtd_theme` では `docutils` のバージョンによって表示が崩れることがあったが，現在は `uv` で固定した組み合わせを使うこと．
