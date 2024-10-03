package com.be.gitfolio.common.config;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;
import lombok.Getter;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;

import java.time.LocalDateTime;

@Getter
@JsonPropertyOrder({"time", "status", "code", "message", "result"})
public class BaseResponse<T> {

    @DateTimeFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss")
    private final LocalDateTime time = LocalDateTime.now();
    private final HttpStatus status;
    private final String code;
    private final String message;

    @JsonInclude(JsonInclude.Include.NON_NULL)
    private T result;


    /**
     * 200 OK 일 떄
     */
    public BaseResponse(T result) {
        this.status = HttpStatus.OK;
        this.code = "200 OK";
        this.message = "요청에 성공했습니다.";
        this.result = result;
    }

    /**
     * 200이 아닌 모든 성공 응답 시
     */
    public BaseResponse(HttpStatus status, String code, String message, T result) {
        this.status = status;
        this.code = code;
        this.message = message;
        this.result = result;
    }
}
