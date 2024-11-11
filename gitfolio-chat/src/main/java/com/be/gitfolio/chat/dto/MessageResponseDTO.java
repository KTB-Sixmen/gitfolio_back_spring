package com.be.gitfolio.chat.dto;

import com.be.gitfolio.chat.domain.ChatMessage;

import java.time.LocalDateTime;

public class MessageResponseDTO {

    public record MessageDetailDTO(
            String chatMessageId,
            Long senderId,
            String senderNickname,
            String content,
            LocalDateTime createdAt
    ) {
        public static MessageDetailDTO from(ChatMessage message) {
            return new MessageDetailDTO(
                    message.getId(),
                    message.getSenderId(),
                    message.getSenderNickname(),
                    message.getContent(),
                    message.getSentAt()
            );
        }
    }
}
