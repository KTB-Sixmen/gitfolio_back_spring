package com.be.gitfolio.chat.dto;

public class MessageRequestDTO {

    public record MessageContentDTO (
        String content
    ) {}
}
