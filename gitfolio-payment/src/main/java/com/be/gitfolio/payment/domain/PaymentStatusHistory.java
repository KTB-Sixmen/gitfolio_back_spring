package com.be.gitfolio.payment.domain;

import com.be.gitfolio.common.config.BaseEntityMySQL;
import com.be.gitfolio.payment.type.PaymentStatus;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Getter
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Builder
public class PaymentStatusHistory {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    @Column(name = "payment_status_history_id")
    private Long id;

    private Long paymentId;

    @Enumerated(EnumType.STRING)
    private PaymentStatus status;

    private LocalDateTime changedAt;    // 변경 일자

    private String reason;  // 상태 변경 사유 (예: 결제 실패 사유)

    /**
     * 정적 팩토리 메서드
     */
    public static PaymentStatusHistory ready(Long paymentId) {
        return PaymentStatusHistory.builder()
                .paymentId(paymentId)
                .status(PaymentStatus.PENDING)
                .changedAt(LocalDateTime.now())
                .reason("결제 준비")
                .build();
    }

    public static PaymentStatusHistory approve(Long paymentId) {
        return PaymentStatusHistory.builder()
                .paymentId(paymentId)
                .status(PaymentStatus.APPROVED)
                .changedAt(LocalDateTime.now())
                .reason("결제 승인")
                .build();
    }

    public static PaymentStatusHistory cancel(Long paymentId, String reason) {
        return PaymentStatusHistory.builder()
                .paymentId(paymentId)
                .status(PaymentStatus.CANCELED)
                .changedAt(LocalDateTime.now())
                .reason(reason)
                .build();
    }

    public static PaymentStatusHistory fail(Long paymentId, String reason) {
        return PaymentStatusHistory.builder()
                .paymentId(paymentId)
                .status(PaymentStatus.FAILED)
                .changedAt(LocalDateTime.now())
                .reason(reason)
                .build();
    }

}
