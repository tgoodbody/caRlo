delayedAssign("gedi", local({
  if (requireNamespace("sf", quietly = TRUE)) {

  } else {
    caRlo:::gedi
  }
}))

delayedAssign("plots", local({
  if (requireNamespace("sf", quietly = TRUE)) {

  } else {
    caRlo:::plots
  }
}))

delayedAssign("samples", local({
  if (requireNamespace("sf", quietly = TRUE)) {

  } else {
    caRlo:::samples
  }
}))
