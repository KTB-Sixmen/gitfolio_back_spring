package com.be.gitfolio.common.type;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum GraduationStatus {

    GRADUATED("졸업"),
    ATTENDING("재학중"),
    DROP_OUT("중퇴"),
    ;

    private final String name;

    public static GraduationStatus fromString(String value) {
        try {
            return GraduationStatus.valueOf(value.toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new BaseException(ErrorCode.ENUM_MAPPING_ERROR);
        }
    }
}
