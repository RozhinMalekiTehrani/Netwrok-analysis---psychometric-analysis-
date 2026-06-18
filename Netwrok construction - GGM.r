# =====================================================================
# 1. LOAD MODULES & CLEAN DATA 
# =====================================================================
library(qgraph)
library(NetworkComparisonTest)

file_path <- "/Users/rozhinmt/Desktop/IPM/Depression-project/hayling /MIX-hayling.csv"
raw_data  <- read.csv(file_path, stringsAsFactors = FALSE)

group_A_raw <- subset(raw_data, EXAMINER_LABEL == "A")
group_B_raw <- subset(raw_data, EXAMINER_LABEL == "B")

features <- c('RTscore', 'brooding', 'R2', 'R1', 'reflection', 'rumination', 
              'anhedonia', 'neuroticism', 'depression', 'anxiety', 'worry', 
              'reappraisal', 'P2')

group_A_clean <- as.data.frame(lapply(group_A_raw[, features], as.numeric))
group_B_clean <- as.data.frame(lapply(group_B_raw[, features], as.numeric))

group_A_clean <- na.omit(group_A_clean)
group_B_clean <- na.omit(group_B_clean)

# =====================================================================
# 2. RUN ALIGNED NCT PERMUTATION TEST (gamma = 0)
# =====================================================================
print("Running aligned permutation test with gamma = 0...")
set.seed(42) # Locks random shuffles for consistent replication

nct_aligned <- NCT(
  data1 = group_A_clean, 
  data2 = group_B_clean, 
  it = 2000, 
  gamma = 0.25,                                              # FIX: Set to 0 to populate Group A
  binary.data = FALSE, 
  test.edges = TRUE, 
  test.centrality = TRUE,
  centrality = c("strength", "betweenness", "closeness"), # Closeness will now calculate safely!
  p.adjust.methods = "fdr"
)

# Print the real, balanced statistical results
print("==== ALIGNED NCT STATISTICS ====")
summary(nct_aligned)

# =====================================================================
# 3. EXTRACT AND VISUALIZE THE EXACT NETWORKS USED IN THE TEST
# =====================================================================
# Pull the exact matrices evaluated inside the nct_aligned object
matrix_A_exact <- nct_aligned$nw1
matrix_B_exact <- nct_aligned$nw2

# Create a locked geometric circle layout based on the variables 
# This ensures nodes do not shift positions between the two plots
fixed_circle_layout <- qgraph(matrix_B_exact, layout = "circle", DoNotPlot = TRUE)$layout

# Set up a side-by-side 1-row, 2-column plotting canvas
par(mfrow = c(1, 2))

# Plot the exact Network A from the test
qgraph(matrix_A_exact, 
       layout = fixed_circle_layout, 
       labels = features, 
       theme = "colorblind", 
       maximum = 0.5,           # Keeps line thickness scale identical across both plots
       vsize = 7,               # Node size
       node.width = 1.2,
       title = "Exact NCT Network: Group A (Strategy)")

# Plot the exact Network B from the test
qgraph(matrix_B_exact, 
       layout = fixed_circle_layout, 
       labels = features, 
       theme = "colorblind", 
       maximum = 0.5, 
       vsize = 7, 
       node.width = 1.2,
       title = "Exact NCT Network: Group B (Non-Strategy)")

# Reset plotting window parameters back to standard single layout
par(mfrow = c(1, 1))

