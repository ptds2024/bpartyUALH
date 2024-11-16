# party_simulation_functions.R

calculate_party_totals <- function(guest_count, cones_per_guest, sd = 0.1) {
  total_cones <- sum(cones_per_guest)
  x_values <- seq(0, 10, length.out = 100)
  random_variations <- generate_random_variations(x_values, sd)
  metrics <- calculate_metrics(random_variations, x_values)
  
  total_volume <- metrics$volume * total_cones
  total_surface_area <- metrics$surface_area * total_cones
  avg_volume <- metrics$volume
  avg_surface_area <- metrics$surface_area
  
  return(list(total_volume = total_volume, 
              total_surface_area = total_surface_area, 
              avg_volume = avg_volume, 
              avg_surface_area = avg_surface_area,
              guest_count = guest_count,
              total_cones = total_cones))
}

simulate_parties <- function(num_parties, forecast_data, sd = 0.1) {
  total_volumes <- numeric(num_parties)
  total_surface_areas <- numeric(num_parties)
  avg_volumes <- numeric(num_parties)
  avg_surface_areas <- numeric(num_parties)
  guest_counts <- numeric(num_parties)
  cone_counts <- numeric(num_parties)
  
  for (i in seq_len(num_parties)) {
    weather <- forecast_data[sample(nrow(forecast_data), 1), ]
    lambda <- calculate_lambda(weather$temperature, weather$humidity, weather$pressure)
    guest_count <- simulate_guest_count(lambda)
    cones_per_guest <- simulate_cones_per_guest(guest_count)
    results <- calculate_party_totals(guest_count, cones_per_guest, sd)
    
    total_volumes[i] <- results$total_volume
    total_surface_areas[i] <- results$total_surface_area
    avg_volumes[i] <- results$avg_volume
    avg_surface_areas[i] <- results$avg_surface_area
    guest_counts[i] <- results$guest_count
    cone_counts[i] <- results$total_cones
  }
  
  return(list(total_volumes = total_volumes,
              total_surface_areas = total_surface_areas,
              avg_volumes = avg_volumes,
              avg_surface_areas = avg_surface_areas,
              guest_counts = guest_counts,
              cone_counts = cone_counts))
}
