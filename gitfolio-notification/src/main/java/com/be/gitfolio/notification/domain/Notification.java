package com.be.gitfolio.notification.domain;

import com.be.gitfolio.common.config.BaseEntityMySQL;
import com.be.gitfolio.common.event.KafkaEvent;
import com.be.gitfolio.common.type.NotificationType;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Notification extends BaseEntityMySQL {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "notification_id")
    private Long id;

    private String resumeId;

    private Long senderId;

    private Long receiverId;

    @Enumerated(EnumType.STRING)
    private NotificationType type;

    @Column(name = "is_read")
    private boolean read;

    public static Notification from(KafkaEvent.ResumeEvent resumeEvent) {
        return Notification.builder()
                .resumeId(resumeEvent.getResumeId())
                .senderId(resumeEvent.getSenderId())
                .receiverId(resumeEvent.getReceiverId())
                .type(resumeEvent.getType())
                .read(Boolean.FALSE)
                .build();
    }

    public void readNotification() {
        this.read = Boolean.TRUE;
    }
}
