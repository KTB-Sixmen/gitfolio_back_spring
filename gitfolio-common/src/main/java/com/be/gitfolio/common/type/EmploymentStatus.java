package com.be.gitfolio.common.type;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum EmploymentStatus {

    RESIGNATION("퇴사"),
    EMPLOYMENT("재직중"),
    ;

    private final String name;

    public static EmploymentStatus fromString(String value) {
        try {
            return EmploymentStatus.valueOf(value.toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new BaseException(ErrorCode.ENUM_MAPPING_ERROR);
        }
    }
}
