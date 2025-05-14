# netsum

`netsum` is an R package and Shiny web application designed to simplify network analysis of behavioral and psychological data.

## Features

- **CSV file upload with automatic validation**  
  Ensures the input data is clean (e.g., no missing values) before proceeding with analysis.

- **Interactive network visualization**  
  Allows users to select variables from the dataset to define network nodes.

- **Customizable graph appearance**, including:
  - Editable node labels
  - Color-coding of node groups
  - Custom network titles

- **Centrality analysis** with multi-selection support:
  - Strength
  - Betweenness
  - Closeness
  - Expected Influence

- **Robustness analysis**  
  Includes diagnostic plots to evaluate network stability.

- **Shortest path finder**  
  Computes and visually highlights the shortest path between any two nodes.

- **Auto-generated summary text**  
  Dynamically generates a written summary (mini results section) of key network results, populated from the analysis (e.g., top central nodes, robustness conclusions).

## Goals

The aim of `netsum` is to make network analysis more accessible, interpretable, and visually engaging without requiring extensive R coding skills.
