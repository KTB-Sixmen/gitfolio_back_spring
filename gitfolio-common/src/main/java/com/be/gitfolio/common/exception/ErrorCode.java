package com.be.gitfolio.common.exception;

import lombok.AllArgsConstructor;
import lombok.Getter;
import org.springframework.http.HttpStatus;

@Getter
@AllArgsConstructor
public enum ErrorCode {

    /**
     * 인증 에러
     */
    UNAUTHORIZED(HttpStatus.UNAUTHORIZED, "인증에 실패했습니다."),

    /**
     * 멤버 에러
     */
    NO_MEMBER_INFO(HttpStatus.NOT_FOUND, "회원 정보가 존재하지 않습니다."),
    BAD_INPUT(HttpStatus.BAD_REQUEST, "입력 형식이 잘못되었습니다."),
    ALREADY_EXIST_MEMBER_ADDITIONAL_INFO(HttpStatus.CONFLICT, "이미 회원의 추가정보가 존재합니다."),
    NO_MEMBER_ADDITIONAL_INFO(HttpStatus.NOT_FOUND, "회원 추가 정보가 존재하지 않습니다."),

    /**
     * 토큰 에러
     */
    TOKEN_IS_MISSING(HttpStatus.UNAUTHORIZED, "토큰이 존재하지 않습니다."),
    EXPIRED_TOKEN(HttpStatus.UNAUTHORIZED, "토큰이 만료되었습니다."),
    TOKEN_NOT_VALID(HttpStatus.UNAUTHORIZED, "토큰이 유효하지 않습니다."),
    TOKEN_CATEGORY_INCORRECT(HttpStatus.UNAUTHORIZED, "토큰의 종류가 맞지 않습니다."),
    TOKEN_NULL(HttpStatus.UNAUTHORIZED, "토큰이 null 입니다."),


    /**
     * 채팅 에러
     */
    CHAT_ROOM_NOT_FOUND(HttpStatus.NOT_FOUND, "채팅방이 존재하지 않습니다."),
    CHAT_MESSAGE_NOT_FOUND(HttpStatus.NOT_FOUND, "채팅 메시지가 존재하지 않습니다."),
    CHAT_SENDER_NOT_EIXIST(HttpStatus.BAD_REQUEST, "메시지를 보낸 사람이 채팅방에 존재하지 않습니다."),

    /**
     * 일반 오류 코드
     */
    BAD_REQUEST(HttpStatus.BAD_REQUEST, "잘못된 요청입니다"),
    NOT_FOUND(HttpStatus.NOT_FOUND, "찾을 수 없습니다"),
    METHOD_NOT_ALLOWED(HttpStatus.METHOD_NOT_ALLOWED, "지원하지 않는 HTTP Method 요청입니다."),
    INTERNAL_SERVER_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, "내부 서버 오류입니다.");

    ;

    private final HttpStatus status;
    private final String message;

}
