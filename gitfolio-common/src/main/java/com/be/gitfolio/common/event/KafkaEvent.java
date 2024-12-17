package com.be.gitfolio.common.event;

import com.be.gitfolio.common.type.NotificationType;
import lombok.*;

public class KafkaEvent {

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor(access = AccessLevel.PROTECTED)
    public static class ResumeEvent {
        private Long senderId;
        private String senderNickname;
        private Long receiverId;
        private String resumeId;
        private NotificationType type;
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor(access = AccessLevel.PROTECTED)
    public static class ExpirationEvent {
        private Long memberId;
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor(access = AccessLevel.PROTECTED)
    public static class MemberDeletedEvent {
        private Long memberId;
    }
}
