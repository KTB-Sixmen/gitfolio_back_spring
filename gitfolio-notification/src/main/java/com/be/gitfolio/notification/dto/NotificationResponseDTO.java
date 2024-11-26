package com.be.gitfolio.notification.dto;

import com.be.gitfolio.common.type.NotificationType;
import com.be.gitfolio.notification.domain.Notification;
import com.fasterxml.jackson.annotation.JsonProperty;

public class NotificationResponseDTO {

    public record NotificationListDTO(
            Long notificationId,
            String resumeId,
            Long senderId,
            String senderNickName,
            Long receiverId,
            NotificationType type,
            @JsonProperty("isRead") boolean read
    ) {
        public static NotificationListDTO from(Notification notification) {
            return new NotificationListDTO(
                    notification.getId(),
                    notification.getResumeId(),
                    notification.getSenderId(),
                    notification.getSenderNickname(),
                    notification.getReceiverId(),
                    notification.getType(),
                    notification.isRead()
            );
        }
    }
}