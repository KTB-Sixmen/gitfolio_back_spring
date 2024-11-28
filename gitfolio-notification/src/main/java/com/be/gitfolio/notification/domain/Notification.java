package com.be.gitfolio.notification.domain;

import com.be.gitfolio.common.config.BaseEntityMySQL;
import com.be.gitfolio.common.event.KafkaEvent;
import com.be.gitfolio.common.type.NotificationType;
import jakarta.persistence.*;
import lombok.*;

import static com.be.gitfolio.common.event.KafkaEvent.*;

@Entity
@Getter
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Builder
public class Notification extends BaseEntityMySQL {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "notification_id")
    private Long id;

    private String resumeId;

    private Long senderId;

    private String senderNickname;

    private Long receiverId;

    @Enumerated(EnumType.STRING)
    private NotificationType type;

    @Column(name = "is_read")
    private boolean read;

    public static Notification from(ResumeEvent resumeEvent) {
        return Notification.builder()
                .resumeId(resumeEvent.getResumeId())
                .senderId(resumeEvent.getSenderId())
                .senderNickname(resumeEvent.getSenderNickname())
                .receiverId(resumeEvent.getReceiverId())
                .type(resumeEvent.getType())
                .read(Boolean.FALSE)
                .build();
    }

    public void readNotification() {
        this.read = Boolean.TRUE;
    }
}
