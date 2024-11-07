package com.be.gitfolio.chat.domain;

import com.be.gitfolio.common.config.BaseEntityMongo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.LocalDateTime;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = "chat_message")
public class ChatMessage {

    @Id
    private String id;
    private String chatRoomId = "globalChatRoom";
    private Long senderId;
    private String senderNickname;
    private String content;
    private LocalDateTime sentAt;
}
