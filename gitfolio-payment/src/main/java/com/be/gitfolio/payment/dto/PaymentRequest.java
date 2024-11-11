package com.be.gitfolio.payment.dto;

import com.be.gitfolio.common.type.PaidPlan;

public class PaymentRequest {

    public record PaidPlanRequest(
            PaidPlan paidPlan
    ) {}

}
