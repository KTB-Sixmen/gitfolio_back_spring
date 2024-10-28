package com.be.gitfolio.common.type;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum WorkType {
    INTERN("인턴"),
    CONTRACT_WORKER("계약직"),
    FULL_TIME("정규직"),
    PRIVATE_BUSINESS("개인사업"),
    FREELANCER("프리랜서")
    ;

    private final String name;

    public static WorkType fromString(String value) {
        try {
            return WorkType.valueOf(value.toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new BaseException(ErrorCode.ENUM_MAPPING_ERROR);
        }
    }
}
