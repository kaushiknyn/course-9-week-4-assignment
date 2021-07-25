
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(shiny)

# Define server logic to display output
shinyServer(function(input, output) {
    
    mortgage <- function(Principal = 500000, Interest = 6, Loan_term = 30, amort = TRUE, plotData = TRUE) {
        Monthly_Interest <- Interest / (12 * 100) #Monthly interest
        Num_of_Periods <- 12 * Loan_term #Number of periods in months
        M <- Principal * Monthly_Interest / (1 - (1 + Monthly_Interest)^(-Num_of_Periods)) #Monthly payment
        monthlyPayment <<- M
        # Calculate Amortization for each Month
        if (amort == TRUE) {
            Pt <- Principal # current principal or amount of the loan
            currP <- NULL
            while (Pt >= 0) {
                H <- Pt * Monthly_Interest # Current monthly interest
                C <- M - H # Monthly payment minus monthly interest, so it is the amount of principal to pay that month
                Q <- Pt - C # this is the new balance of principal of the loan
                Pt <- Q # sets Principal equal to Q and goes back to step 1. The loop continues until the value Q (and hence Principal) goes to zero
                currP <- c(currP, Pt)
            }
            monthP <- c(Principal, currP[1:(length(currP) - 1)]) - currP
            Payment_In_Month <<- data.frame(
                Month = 1:length(currP),
                Year = sort(rep(1:ceiling(Num_of_Periods / 12), 12))[1:length(monthP)],
                Balance = c(currP[1:(length(currP))]),
                Payment = monthP + c((monthlyPayment - monthP)[1:(length(monthP))]),
                Principal = monthP,
                Interest = c((monthlyPayment - monthP)[1:(length(monthP))])
            )
            Payment_In_Month <<- subset(Payment_In_Month, Year <= Loan_term * 12)
            Payment_In_Year <- data.frame(
                Amortization = tapply(Payment_In_Month$Balance, Payment_In_Month$Year, max),
                Annual_Payment = tapply(Payment_In_Month$Payment, Payment_In_Month$Year, sum),
                Annual_Principal = tapply(Payment_In_Month$Principal, Payment_In_Month$Year, sum),
                Annual_Interest = tapply(Payment_In_Month$Interest, Payment_In_Month$Year, sum),
                Year = as.factor(na.omit(unique(Payment_In_Month$Year)))
            )
            Payment_In_Year <<- Payment_In_Year
        }
        if (plotData == TRUE) {
            aDFyear_2 <- Payment_In_Year %>%
                rename(
                    Interest = Annual_Interest,
                    Payment = Annual_Payment,
                    Principal = Annual_Principal
                )
            aDFyear_2$Year <- as.factor(aDFyear_2$Year)
            aDFyear_2 <- melt(aDFyear_2[, c("Interest", "Principal", "Year")], id.vars = "Year")
            
            ggplot(aDFyear_2, aes(x = Year, y = value, fill = variable)) +
                geom_bar(position = "fill", stat = "identity") +
                labs(y = "Payment") +
                scale_y_continuous(labels = percent) +
                theme_minimal() +
                theme(legend.title = element_blank(), legend.position = "top")
        }
    }
    
    output$text <- renderUI({
        mortgage(Principal = input$principal, Interest = input$interest, Loan_term = input$length, plotData = FALSE)
        HTML(paste0(
            "<h3>", "Summary", "</h3>",
            "<b>","Principal (loan amount): ", "</b>", "$",format(round(input$principal, 2), big.mark = ",", scientific = FALSE),
            "<br>",
            "<b>", "Annual interest rate: ", "</b>", input$interest, "%",
            "<br>",
            "<b>","Term: ","</b>", input$length, " years (", input$length * 12, " months)",
            "<br>",
            "<b>", "Monthly payment: ", "</b>","$",format(round(monthlyPayment, digits = 2), big.mark = ","), "</b>",
            "<br>",
            "<b>", "Total cost: ", "</b>", "$",format(round(input$principal, 2), big.mark = ",", scientific = FALSE), "     (principal) + ", "$",format(round(monthlyPayment * 12 * input$length - input$principal, 2), big.mark = ","), " (interest) = ", "<b>", "$",format(round(monthlyPayment * 12 * input$length, digits = 2), big.mark = ","), "</b>"
        ))
    })
    
    output$distPlot <- renderPlot({
        mortgage(Principal = input$principal, Interest = input$interest, Loan_term = input$length, plotData = input$plot)
    })


})
