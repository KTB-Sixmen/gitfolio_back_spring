package com.be.gitfolio.common.type;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum PaidPlan {
    FREE("무료 플랜", 0),
    PRO("프로 플랜", 22500),
    ;

    private String planName;
    private int cost;

    public static PaidPlan fromPlanName(String planName) {
        for (PaidPlan plan : PaidPlan.values()) {
            if (plan.getPlanName().equals(planName)) {
                return plan;
            }
        }
        throw new BaseException(ErrorCode.ENUM_MAPPING_ERROR);
    }
}