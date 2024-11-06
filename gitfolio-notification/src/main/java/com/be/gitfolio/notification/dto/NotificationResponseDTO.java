package com.be.gitfolio.notification.dto;

import com.be.gitfolio.common.type.NotificationType;
import com.be.gitfolio.notification.domain.Notification;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

public class NotificationResponseDTO {

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class NotificationListDTO {
        private Long notificationId;
        private String resumeId;
        private Long senderId;
        private Long receiverId;
        private NotificationType type;
        @JsonProperty("isRead")
        private boolean read;

        public static NotificationListDTO from(Notification notification) {
            return NotificationListDTO.builder()
                    .notificationId(notification.getId())
                    .resumeId(notification.getResumeId())
                    .senderId(notification.getSenderId())
                    .receiverId(notification.getReceiverId())
                    .type(notification.getType())
                    .read(notification.isRead())
                    .build();
        }
    }
}
