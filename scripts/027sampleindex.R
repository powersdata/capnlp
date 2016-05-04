for (j in 1:word_length) {
        tmp_word <- word[j]
        tmp_idx <- grep(tmp_word, orig[[1]]$content, ignore.case = TRUE, perl = TRUE)
        idx <- sort(unique(as.integer(rbind(tmp_idx, idx))))
}

assign(paste0(i, "_idx"), idx, envir = .GlobalEnv)