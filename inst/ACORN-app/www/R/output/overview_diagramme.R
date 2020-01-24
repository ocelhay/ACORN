# output$diagramme <- renderGrViz({
#   req(patient())
#   req(microbio())
# 
#   cai_micro <- length(unique(
#     intersect(microbio()$patient_id,
#               patient() %>% filter(surveillance_cat == "CAI") %>% pull(patient_id)
#     )))
# 
#   cai_no_micro <- length(unique(
#     setdiff(patient() %>% filter(surveillance_cat == "CAI") %>% pull(patient_id),
#             microbio()$patient_id
#     )))
# 
#   cai_co <- patient() %>% filter(surveillance_cat == "CAI", clinical_outcome) %>% nrow()
#   cai_d28o <- patient() %>% filter(surveillance_cat == "CAI", d28_outcome) %>% nrow()
#   cai_noo <- patient() %>% filter(surveillance_cat == "CAI", !clinical_outcome, !d28_outcome) %>% nrow()
# 
#   hai_micro <- length(unique(
#     intersect(microbio()$patient_id,
#               patient() %>% filter(surveillance_cat == "HAI") %>% pull(patient_id)
#     )))
# 
#   hai_no_micro <- length(unique(
#     setdiff(patient() %>% filter(surveillance_cat == "HAI") %>% pull(patient_id),
#             microbio()$patient_id
#     )))
# 
#   hai_co <- patient() %>% filter(surveillance_cat == "HAI", clinical_outcome) %>% nrow()
#   hai_d28o <- patient() %>% filter(surveillance_cat == "HAI", d28_outcome) %>% nrow()
#   hai_noo <- patient() %>% filter(surveillance_cat == "HAI", clinical_outcome, !d28_outcome) %>% nrow()
# 
#   nodes <- create_node_df(
#     n = 7,
#     type = "a",
#     width = c(1, 1.4, 1.4, 1.5, 1.5, 1.5, 1.5),
#     # cluster = c(NA, NA, NA, "CAI", "CAI"),
#     label = c(paste0("Patient Enrollments \n (n = ", nrow(patient()), ")"),
#               paste0("Community Acquired Infections \n (n = ", nrow(patient() %>% filter(surveillance_cat == "CAI")), ")"),
#               paste0("Hospital Acquired Infections \n (n = ", nrow(patient() %>% filter(surveillance_cat == "HAI")), ")"),
#               paste0("{With microbiology (n = ", cai_micro, ")| No microbiology (n = ", cai_no_micro, ")}"),
#               paste0("{With Clinical Outcome (n = ", cai_co, ") | With D28 Outcome (n = ", cai_d28o, ") | No Outcome (n = ", cai_noo, ")}"),
#               paste0("{With microbiology (n = ", hai_micro, ")| No microbiology (n = ", hai_no_micro, ")}"),
#               paste0("{With Clinical Outcome (n = ", hai_co, ") | With D28 Outcome (n = ", hai_d28o, ") | No Outcome (n = ", hai_noo, ")}")),
#     style = "filled",
#     color = c("#7f8c8d", rep("#2980b9", 2), rep("#95a5a6", 4)),
#     fillcolor = c("#bdc3c7", rep("#3498db", 2), rep("#2c3e50", 4)),
#     shape = c("box", "folder", "folder", "record", "record", "record", "record"))
# 
#   edges <- create_edge_df(
#     from = c(1, 1, 2, 2, 3, 3),
#     to = c(2, 3, 4, 5, 6, 7),
#     color = "black",
#     arrowhead = c("normal", "normal", "tee", "tee", "tee", "tee"),
#     width = c(1, 1, 1, 1)
#   )
# 
#   graph <- create_graph(nodes_df = nodes, edges_df = edges) %>%
#     set_node_position(node = 1, x = 2, y = 2.2) %>%
#     set_node_position(node = 2, x = 1, y = 1.3) %>%
#     set_node_position(node = 3, x = 3, y = 1.3) %>%
#     set_node_position(node = 4, x = 0.5, y = 0.6) %>%
#     set_node_position(node = 5, x = 0.9, y = 0) %>%
#     set_node_position(node = 6, x = 2.8, y = 0.6) %>%
#     set_node_position(node = 7, x = 3.2, y = 0) %>%
#     add_global_graph_attrs(attr = "penwidth", value = 0.8, attr_type = "node") %>%
#     add_global_graph_attrs(attr = "fontsize", value = 7, attr_type = "node") %>%
#     add_global_graph_attrs(attr = "arrowsize", value = 1, attr_type = "edge") %>%
#     add_global_graph_attrs(attr = "splines", value = "ortho", attr_type = "graph")
# 
#   generate_dot(graph)
# })
