[![Contributors][contributors-shield]][contributors-url]  
[![Forks][forks-shield]][forks-url]  
[![Stargazers][stars-shield]][stars-url]  
[![Issues][issues-shield]][issues-url]  
[![MIT License][license-shield]][license-url]  
[![LinkedIn][linkedin-shield]][linkedin-url]  

<!-- PROJECT LOGO -->
<div align="center" id="readme-top">
  <a href="https://github.com/tusharad/sql2er">
    <img src="example/logo.jpeg" alt="Logo" width="180" height="180">
  </a>

  <h1 align="center">SQL 2 ER</h1>

  <p align="center">
    A command-line tool to convert SQL scripts into Entity-Relationship (ER) diagrams.
    <br>
    Designed to work with PostgreSQL syntax.
    <br>
    <a href="https://github.com/tusharad/sql2er/issues/new?labels=bug&template=bug-report---.md"><strong>Report a Bug</strong></a>
    ·
    <a href="https://github.com/tusharad/sql2er/issues/new?labels=enhancement&template=feature-request---.md"><strong>Request a Feature</strong></a>
  </p>
</div>

---

## Table of Contents

- [Example](#example)  
- [Getting Started](#getting-started)  
- [Built With](#built-with)  
- [Roadmap](#roadmap)  
- [Limitations](#limitations)  
- [Unsupported Features](#unsupported-features)  
- [Acknowledgments](#acknowledgments)

---

## Example

**Input: `test.sql`**

```sql
CREATE TABLE department (
    dep_id SERIAL PRIMARY KEY, 
    dep_name VARCHAR(30), 
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE employee (
    employee_id SERIAL PRIMARY KEY, 
    employee_name VARCHAR(30), 
    employee_age INT, 
    dep_id INT REFERENCES department (dep_id) ON DELETE CASCADE, 
    created_at TIMESTAMPTZ DEFAULT NOW()
);

CREATE TABLE tasks (
    task_id INT, 
    task_name TEXT
);
```

**Command:**

```bash
./sql2er-exe test.sql -o erd.svg
```

**Output:**

<img src="example/erd.svg" alt="ER Diagram" width="100%" height="580">

---

## Getting Started

### Option 1: Download Binary

1. Download the binary from the [Releases](https://github.com/tusharad/sql2er/releases) page.
2. Run the tool:

    ```bash
    ./sql2er-exe test.sql -o erd.svg
    ```

### Option 2: Build from Source

1. Install [Stack](https://docs.haskellstack.org/en/stable/) via [GHCup](https://www.haskell.org/ghcup/).
2. Clone the repository and navigate to the project root.
3. Build and run the project:

    ```bash
    stack run -- test.sql -o erd.svg
    ```

---

## Built With

[![Haskell][Haskell]][Haskell-url]

<p align="right">(<a href="#readme-top">back to top</a>)</p>

---

## Roadmap

- [x] Add Changelog  
- [x] Add Test Cases  
- [x] Support `GENERATED` Constraint  
- [x] Gracefully Ignore Partitions  
- [x] Support `bigserial`  
- [ ] Add Additional Examples  
- [ ] Enhance Documentation  
- [ ] Add More Parsing Functions  
- [ ] Support Interval Data Type  
- [ ] Support 2D Arrays  

For the full list of proposed features and known issues, check out the [open issues](https://github.com/tusharad/sql2er/issues).

<p align="right">(<a href="#readme-top">back to top</a>)</p>

---

## Limitations

- **Syntax Validation:**  
  The parser doesn't validate SQL syntax; it extracts only the necessary information for generating ER diagrams.  
- **Foreign Key Constraints:**  
  Currently supports single-column foreign keys only.  
- **PostgreSQL Specific:**  
  Designed and tested using PostgreSQL 17.  
- **Function Parsing:**  
  Parsing stops at `CREATE FUNCTION` statements.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

---

## Unsupported Features

- `DETACH`  
- `USING ...`  
- `TABLESPACE`  
- `NOT VALID`  
- `VALIDATE`  
- `INTERVAL` Data Type  

<p align="right">(<a href="#readme-top">back to top</a>)</p>

---

## Acknowledgments

This project was inspired by [sqldiagram](https://github.com/RadhiFadlillah/sqldiagram), which focuses on MySQL but lacked robust parsing capabilities.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- MARKDOWN LINKS & IMAGES -->
[contributors-shield]: https://img.shields.io/github/contributors/tusharad/sql2er.svg?style=for-the-badge
[contributors-url]: https://github.com/tusharad/sql2er/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/tusharad/sql2er.svg?style=for-the-badge
[forks-url]: https://github.com/tusharad/sql2er/network/members
[stars-shield]: https://img.shields.io/github/stars/tusharad/sql2er.svg?style=for-the-badge
[stars-url]: https://github.com/tusharad/sql2er/stargazers
[issues-shield]: https://img.shields.io/github/issues/tusharad/sql2er.svg?style=for-the-badge
[issues-url]: https://github.com/tusharad/sql2er/issues
[license-shield]: https://img.shields.io/github/license/tusharad/sql2er.svg?style=for-the-badge
[license-url]: https://github.com/tusharad/sql2er/blob/main/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/tushar-adhatrao
[Haskell]: https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white
[Haskell-url]: https://www.haskell.org/
