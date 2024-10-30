package com.be.gitfolio.payment.dto;

import com.be.gitfolio.common.type.PaidPlan;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

public class PaymentRequest {

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class PaidPlanRequest {
        private PaidPlan paidPlan;
    }

}
