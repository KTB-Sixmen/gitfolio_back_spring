package com.be.gitfolio.common.type;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum PaidPlan {
    FREE("무료 플랜", 0),
    BASIC("베이직 플랜", 9800),
    STANDARD("스탠다드 플랜", 14700),
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