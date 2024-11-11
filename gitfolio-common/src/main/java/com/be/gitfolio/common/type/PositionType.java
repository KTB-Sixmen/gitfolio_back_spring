package com.be.gitfolio.common.type;

import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.exception.ErrorCode;
import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public enum PositionType {

    BACKEND("백엔드"),
    FRONTEND("프론트엔드"),
    INFRA("인프라"),
    AI("인공지능"),
    GAME("게임"),
    ;

    private final String name;

    public static PositionType fromString(String value) {
        try {
            return PositionType.valueOf(value.toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new BaseException(ErrorCode.ENUM_MAPPING_ERROR);
        }
    }
}
