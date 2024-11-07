package com.be.gitfolio.chat.domain;

import com.be.gitfolio.common.config.BaseEntityMongo;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = "chat_room")
public class ChatRoom {

    @Id
    private String id = "globalChatRoom";
    private Set<Long> participants = new HashSet<>();

    public ChatRoom(String globalChatRoom) {
        this.id = globalChatRoom;
    }

    // 참여자 추가
    public void addParticipant(Long memberId) {
        participants.add(memberId);
    }

    // 참여자 제거
    public void removeParticipant(Long memberId) {
        participants.remove(memberId);
    }

    // 참여자 수 조회
    public int getParticipantCount() {
        return participants.size();
    }


}
