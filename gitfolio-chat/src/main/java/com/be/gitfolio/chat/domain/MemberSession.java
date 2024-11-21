package com.be.gitfolio.chat.domain;

import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.LocalDateTime;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Document(collection = "member_session")
public class MemberSession {

    @Id
    private String id;
    private Long memberId;
    private LocalDateTime joinedAt;
    private LocalDateTime leftAt;

    public static MemberSession from(Long memberId) {
        return MemberSession.builder()
                .memberId(memberId)
                .joinedAt(LocalDateTime.now())
                .leftAt(null)
                .build();
    }

    //채팅방 퇴장시 호출
    public void markAsLeft() {
        this.leftAt = LocalDateTime.now();
    }

    // 이미 있는 세션에 입장시간 업데이트
    public void updateJoinedAt() {
        this.joinedAt = LocalDateTime.now();
        this.leftAt = null;
    }

    // 세션 활성 상태 확인(leftAt이 null이면 활성화 상태)
    public boolean isActive() {
        return leftAt == null;
    }
}
