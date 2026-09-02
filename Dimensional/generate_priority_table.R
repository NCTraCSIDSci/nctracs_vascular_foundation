generate_priority_table <- function(feature_table,
    concept_table)
{
    gc()
    filtered_first_priority <- feature_table %>%
        filter(concept_date < first_revasc | is.na(first_revasc))
    gc()
    filtered_first_priority <- filtered_first_priority %>%
        select(person_id,
            concept_id,
            first_revasc,
            last_revasc,
            first_amp,
            last_amp,
            death_date)
    filtered_first_priority <- filtered_first_priority %>%
        distinct()
    gc()
    filtered_first_priority <- filtered_first_priority %>%
        group_by(concept_id) %>%
        summarize(
            Total = n(),
            Untreated = sum(ifelse(is.na(first_revasc), 1, 0)),
            Treated = sum(ifelse(!is.na(first_revasc), 1, 0)),
            Unamputated = sum(ifelse(is.na(first_amp) | last_amp <= first_revasc, 1, 0)), # Effectively filters amputations by first_revasc date.
            Amputated = sum(ifelse(!is.na(first_amp) & last_amp > first_revasc, 1, 0)),
            Alive = sum(ifelse(is.na(death_date), 1, 0)),
            Dead = sum(ifelse(!is.na(death_date), 1, 0)),
            Untreated_alive = sum(ifelse(is.na(first_revasc) & is.na(death_date), 1, 0)),
            Treated_alive = sum(ifelse(!is.na(first_revasc) & is.na(death_date), 1, 0)),
            Untreated_dead = sum(ifelse(is.na(first_revasc) & !is.na(death_date), 1, 0)),
            Treated_dead = sum(ifelse(!is.na(first_revasc) & !is.na(death_date), 1, 0)),
            Untreated_unamp = sum(ifelse(is.na(first_revasc) & (is.na(first_amp) | last_amp <= first_revasc), 1, 0)),
            Treated_unamp = sum(ifelse(!is.na(first_revasc) & (is.na(first_amp) | last_amp <= first_revasc), 1, 0)),
            Untreated_amp = sum(ifelse(is.na(first_revasc) & (!is.na(first_amp) & last_amp > first_revasc), 1, 0)),
            Treated_amp = sum(ifelse(!is.na(first_revasc) & (!is.na(first_amp) & last_amp > first_revasc), 1, 0))
        ) %>%
        ungroup() %>%
        distinct() %>%
        mutate(Death = Dead / (Dead + Alive) / mean(Dead / (Dead + Alive)),
               Treatment = Treated / (Treated + Untreated) / mean(Treated / (Treated + Untreated)),
               Amputation = Amputated / (Amputated + Unamputated) / mean(Amputated / (Amputated + Unamputated)),
               Treat_diff = Treated / (Treated + Untreated),
               Outcome_diff_TD = (Treated_alive * (Untreated_alive + Untreated_dead)) / (Untreated_alive * (Treated_alive + Treated_dead) + 1e-6) / mean((Treated_alive * (Untreated_alive + Untreated_dead)) / (Untreated_alive * (Treated_alive + Treated_dead) + 1e-6)),
               Outcome_diff_TA = (Treated_unamp * (Untreated_unamp + Untreated_amp)) / (Untreated_unamp * (Treated_unamp+ Treated_amp) + 1e-6) / mean((Treated_unamp * (Untreated_amp + Untreated_unamp)) / (Untreated_unamp * (Treated_unamp + Treated_amp) + 1e-6))) %>%
        mutate(Salient_D = pmax(Death, 1 / (Death + 1e-6)),
               Salient_T = pmax(Treatment, 1 / (Treatment + 1e-6)),
               Salient_A = pmax(Treatment, 1 / (Treatment + 1e-6)),
               Salient_TD = pmax(Outcome_diff_TD, 1 / (Outcome_diff_TD + 1e-6)),
               Salient_TA = pmax(Outcome_diff_TA, 1 / (Outcome_diff_TA + 1e-6))) %>%
        arrange(-Salient_TD) %>%
        collect()
    gc()
    arranged_priority <- filtered_first_priority %>%
        arrange(-Salient_TD) %>%
        select(concept_id,
               Salient_A,
               Salient_D,
               Salient_T,
               Salient_TD,
               Salient_TA,
               Outcome_diff_TD,
               Outcome_diff_TA,
               Treat_diff,
               Treated,
               Total) %>%
        inner_join(concept_table %>% select(concept_id, concept_name)) %>%
        distinct()
    return(arranged_priority)
}
