#' Configuration parameters for health data case study
#' This file contains all parameters used in the synthetic data generation process.

CONFIG <- list(
    # Basic parameters
    basic = list(
        seed = 42,
        n_users = 5000,
        n_campaigns = 3,
        n_fact_records = 10000
    ),
    
    # Time range parameters
    dates = list(
        start = "2020-01-01",
        end = "2024-01-01",
        activation_hours = 8:20  # Hours when users typically activate
    ),
    
    # Demographic parameters
    demographics = list(
        sex = c(
            female = 0.70, 
            male = 0.28, 
            unknown = 0.02
        ),
        age_groups = c(
            "18-24" = 0.10, 
            "25-34" = 0.15, 
            "35-44" = 0.35, 
            "45-54" = 0.30, 
            "55+" = 0.10
        ),
        population_density = list(
            high = list(range = 3000:5000, prop = 0.20),
            medium = list(range = 1000:2999, prop = 0.15),
            low = list(range = 5:999, prop = 0.65)
        )
    ),
    
    # Campaign parameters
    campaigns = list(
        names = c("Basic", "Pro", "Advanced"),
        activities = c(15, 30, 60),
        experimental_prop = 0.7,  # 70% of users in experimental group
        buffer_days = 60         # Days after activation before campaign starts
    ),
    
    # Step count parameters
    steps = list(
        weekday = list(min = 6000, max = 12000),
        weekend = list(min = 4000, max = 8000),
        random_variation = -1000:1000,
        
        seasonal = list(
            winter = list(
                months = c(12, 1, 2),
                multiplier = seq(0.7, 0.9, 0.1)
            ),
            summer = list(
                months = c(6, 7, 8),
                multiplier = seq(1.1, 1.3, 0.1)
            )
        ),
        
        campaign_effect = list(
            during = seq(1.2, 1.4, 0.1),  # 20-40% increase
            after = seq(1.1, 1.3, 0.1)    # 10-30% increase
        ),
        
        missing_data_prop = 0.8  # Keep 80% of days
    )
)