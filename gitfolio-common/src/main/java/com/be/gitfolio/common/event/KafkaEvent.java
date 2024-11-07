package com.be.gitfolio.common.event;

import com.be.gitfolio.common.type.NotificationType;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

public class KafkaEvent {

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class ResumeEvent {
        private Long senderId;
        private Long receiverId;
        private String resumeId;
        private NotificationType type;
    }
}
