## Often, I have broken the function for the case where only one clonotype is
## provided, because it needs a different paste command.

test_yassai_identifier <- function() {
    checkIdentical(
        "aAYt.22A14N2A49L10",
        yassai_identifier(
            c(
                lib          = "A",
                V            = "TRAV14N-2",
                J            = "TRAJ49",
                score        = "226",
                mapq         = "32",
                read         = "GWYEMNS05C5QZK",
                dna          = "GCAGCCTACACGGGTTACCAGAACTTCTAT",
                qual         = "IIIIIIIIIHHHHHIIIIIIIGEEECD6@1",
                pep          = "AAYTGYQNFY",
                unproductive = FALSE
            )
        )
    )
}
