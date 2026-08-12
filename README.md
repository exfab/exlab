<p align="center">
  <img src="assets/exlab_v3.png" alt="exlab" width="300" />
</p>

A lightweight, high-performance Laboratory Information Management System (LIMS)
for synthetic biology and automated high-throughput labs. It tracks projects,
strains, samples, plates and experimental results in one place, and exposes a
REST API so both humans (via the web UI) and lab automation can work with the
same data.

The backend is written in OCaml for type safety, with a minimal Vanilla JS
frontend and PostgreSQL for storage.

<p align="center">
  <a href="LICENSE"><img alt="License" src="https://img.shields.io/github/license/exfab/exlab"></a>
  <a href="https://www.ocaml.org/"><img alt="OCaml" src="https://img.shields.io/badge/OCaml-5.2%2B-EA8220?logo=ocaml"></a>
  <a href="https://dune.build/"><img alt="Dune" src="https://img.shields.io/badge/Dune-3.22-C9A227"></a>
  <a href="https://www.postgresql.org/"><img alt="PostgreSQL" src="https://img.shields.io/badge/PostgreSQL-15%2B-336791?logo=postgresql&logoColor=white"></a>
</p>

---

## Quick start (how to use it)

### Option A: Docker (fastest)

```bash
docker compose up --build
```

This starts the app (port **8080**), PostgreSQL (port **5433**) and Adminer
(port **8090**). Open <http://localhost:8080> and log in.

### Option B: Local install

Prerequisites: OCaml 5.x (via opam), `dune`, PostgreSQL 15+.

```bash
git clone https://github.com/exfab/exlab.git
cd exlab
opam install . --deps-only
export DATABASE_URL=postgresql://user:password@localhost:5433/exlab_dev
dune exec exlab
```

Then open <http://localhost:8080> and log in.

### Logging in

On first run the database is migrated automatically. If no users exist, a
default administrator account is created:

- Email: `admin@exlab.com`
- Password: `admin123`

Override these with the `DEFAULT_ADMIN_EMAIL` and `DEFAULT_ADMIN_PASSWORD`
environment variables. To start with example projects, strains and plates,
set `AUTO_POPULATE_TEST_DATA=true` (Docker Compose enables this by default).

> **Trying it with sample data:** point your browser at the `/docs` page for an
> interactive API reference, and see `examples/` for CSV files you can bulk
> import through the UI.

---

## What it can do

### Core data model

- **Projects** – top-level grouping for experiments and team management; each
  project can have its own team of users and a dashboard.
- **Strains** – organism records with lineage and cross-references to external
  databases (e.g. NCBI).
- **Samples** – physical or virtual biological materials, linked to projects
  and strains, with optional parent/child (lineage) relationships.
- **Plates & Wells** – multi-well plates (24, 48, 96, 384-well) with
  well-coordinate mapping and dashboard views.
- **Results** – extensible experimental measurements built from
  **categories** and **definitions** (e.g. an "OD600" definition), recorded
  against samples or plates.
- **Products** – a catalog of standard labware and reagents.

### Working with plates

- Create plates and assign/unassign samples to wells, by coordinate or by
  bulk operations.
- **Layouts**: upload a plate layout as an alphanumeric list (`A1 → sample`),
  a numeric list, or a visual matrix grid (see `examples/plate_layout*.csv`).
- **Plate planner** and **multi-plate planner**: plan new plates / transfers
  and generate a transfer map in a few steps.
- **Auto-fill** wells, bulk-create multiple plates at once, and export a
  plate's data.

### Recording results

- Define result categories and result definitions (with a `short_id` used in
  bulk uploads).
- Add results against individual samples or entire plates.
- Bulk-import results from CSV (see `examples/example_results*.csv`).
- Export results for a project.

### Bulk import & export

ExLab supports CSV (and some JSON) for high-throughput workflows. Ready-made
templates live in `examples/`:

| File | Purpose |
|------|---------|
| `example_products.csv` | Bulk-import labware and reagents |
| `example_strains.csv` | Bulk-import strains |
| `example_strains.json` | Bulk-import strains with external DB links |
| `example_samples.csv` | Bulk-create samples within a project |
| `example_results.csv` | Bulk-import results against samples |
| `example_results_plate.csv` | Bulk-import results against plates |
| `plate_layout*.csv` | Set plate layouts (list, numeric, or matrix) |
| `bulk_plate_creation.csv` | Generate multiple plates + assign samples in one upload |

### Access control

Role-based access control (RBAC) with four roles — **Admin**, **Lab Manager**,
**Project Manager**, **Project User** — enforced through encrypted cookie
sessions and email/password authentication (Argon2 hashing).

### API

The server exposes a versioned REST API under `/api/v1` covering all resources
above. An interactive reference is served at `/docs` (OpenAPI/Scalar), and the
spec is available at `/openapi.yaml`.

---

## Project layout

```
src/core/      Domain types, pure logic, serialization
src/storage/   PostgreSQL access (Caqti) and code-based migrations
src/server/    Dream web framework, REST routes, auth middleware
bin/           Executable entry point (DB wait → migrate → seed → serve)
test/          Unit and integration test suites
examples/      CSV/JSON templates for bulk import
```

---

## Development

```bash
dune build                 # build everything
dune exec exlab            # run the server
dune runtest -f            # run all tests (needs PostgreSQL on localhost:5433)
dune fmt                   # format with ocamlformat
dune build @fmt            # check formatting without writing
```

Environment variables (with defaults) are documented in `AGENTS.md`.

---

## Documentation

The API is documented with an OpenAPI specification, served as an interactive
reference inside the running app. Start the server, then visit
<http://localhost:8080/docs> (or the `/docs` link in the frontend navigation)
to browse and try every endpoint. The raw spec is available at
`/openapi.yaml`. Sample data and CSV templates for bulk import live in
`examples/`.

---

## Funding

<p align="center">
  <img src="assets/exfab_combined_awardnumber.png" alt="NSF ExFAB BioFoundry" width="400" />
</p>

This material is based on work performed at the NSF ExFAB BioFoundry and
supported by the National Science Foundation under Award No. DBI-2400327.
ExLab is developed in collaboration between UC Santa Barbara and UC Riverside.
