package com.be.gitfolio.payment.type;


public enum PaymentStatus {
    PENDING,      // 결제 대기 중
    APPROVED,     // 결제 승인됨
    FAILED,       // 결제 실패
    CANCELED,     // 결제 취소됨
    REFUNDED      // 환불 완료
}
