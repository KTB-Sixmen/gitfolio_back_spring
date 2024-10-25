package com.be.gitfolio.common.type;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum SchoolType {

    PRIVATE_EDUCATION("사설 교육"),
    HIGH_SCHOOL("고등학교"),
    UNIVERSITY_ASSOCIATE_DEGREE("대학교(전문학사)"),
    UNIVERSITY_BACHELOR("대학교(학사)"),
    GRADUATE_SCHOOL_MASTER("대학원(석사)"),
    GRADUATE_SCHOOL_DOCTOR("대학원(박사)"),
    ;

    private final String name;

    public static SchoolType fromString(String value) {
        try {
            return SchoolType.valueOf(value.toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new BaseException(ErrorCode.ENUM_MAPPING_ERROR);
        }
    }
}
