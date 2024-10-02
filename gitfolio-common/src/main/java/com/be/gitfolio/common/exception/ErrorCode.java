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
    LOGIN_FAILED(HttpStatus.BAD_REQUEST, "사용자 로그인에 실패했습니다."),
    BAD_INPUT(HttpStatus.BAD_REQUEST, "입력 형식이 잘못되었습니다."),
    NO_USER_INFO(HttpStatus.NOT_FOUND, "사용자 정보가 존재하지 않습니다."),
    EXISTING_USER_INFO(HttpStatus.CONFLICT, "이미 존재하는 사용자입니다."),

    /**
     * 토큰 에러
     */
    NO_TOKEN_CONTENT(HttpStatus.BAD_REQUEST, "토큰의 내용을 가져오지 못했습니다."),
    EXPIRED_TOKEN(HttpStatus.UNAUTHORIZED, "토큰이 만료되었습니다."),
    TOKEN_NOT_VALID(HttpStatus.UNAUTHORIZED, "토큰이 유효하지 않습니다."),
    TOKEN_CATEGORY_INCORRECT(HttpStatus.BAD_REQUEST, "토큰의 종류가 맞지 않습니다."),

    /**
     * 상품 에러
     */
    NOT_EXIST_PRODUCT(HttpStatus.NOT_FOUND, "존재하지 않는 상품입니다."),

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
