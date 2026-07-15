# exlab

A lightweight, high-performance Laboratory Information Management System (LIMS) designed for synthetic biology and automated high-throughput laboratories. 

## Overview

exlab features an OCaml-native backend for type safety and data integrity, paired with a minimal Vanilla JS frontend. It uses PostgreSQL for persistent storage and provides a unified interface for both human operators and lab automation equipment.

## Technology Stack

- **Backend:** OCaml 5.2+
- **Web Framework:** Dream
- **Database:** PostgreSQL 15+
- **Frontend:** Vanilla JS / HTML5 / CSS3
- **Serialization:** Yojson / Ppx_yojson_conv
- **Build System:** Dune

## Core Data Models

- **Projects:** Top-level grouping for experiments and team management.
- **Strains:** Database of organisms with lineage and external database cross-references (e.g. NCBI).
- **Samples:** Physical or virtual biological materials linked to projects and strains.
- **Plates & Wells:** Management for multi-well plates (24, 48, 96, 384-well) and well-coordinate mapping.
- **Results:** Extensible experimental measurements (values, definitions, and categories).
- **Products:** Catalog of standard labware and reagents.

## Access Control

Security is managed via a Role-Based Access Control (RBAC) system implemented through encrypted cookie sessions.

- **Roles:** Admin, Lab Manager, Project Manager, Project User.
- **Authentication:** Email/Password (Argon2 hashing).
  

## Getting Started

### Prerequisites
- OCaml 5.x (via opam)
- PostgreSQL
- `dune` build system 

### Installation
1. **Clone the repository:**
   ```bash
   git clone https://github.com/exfab/exlab.git
   cd exlab
   ```
2. **Install Dependencies:**
   ```bash
   opam install . --deps-only
   ```
3. **Configure Database:**
   Set `DATABASE_URL` in your environment or a local `.env` file:
   `DATABASE_URL=postgresql://user:password@localhost:5433/exlab_dev`

4. **Run the Server:**
   ```bash
   dune exec exlab
   ```
   *Note: On its first run, the system automatically initializes the database and creates a default administrator account if none exists. You can customize these via `DEFAULT_ADMIN_EMAIL` and `DEFAULT_ADMIN_PASSWORD` environment variables. Setting `AUTO_POPULATE_TEST_DATA=true` will also seed the database with example laboratory data (projects, strains, and plates).*

### Running with Docker

ExLab provides a Docker Compose configuration to simplify deployment and development.

- **Start the full stack (App, PostgreSQL, and Adminer):**
  ```bash
  docker compose up --build
  ```

*Note: You can customize the default admin credentials directly in the `Dockerfile` before building.*

## Documentation
*   **`docs/DEPLOYMENT.md`:** Instructions for production deployment on Google Cloud.
*   **`docs/data_structure.md`:** Comprehensive API documentation.
*   **`docs/curl_commands.md`:** Example API interactions.


## Acknowledgements

All emojis designed by OpenMoji – the open-source emoji and icon project. License: CC BY-SA 4.0.
