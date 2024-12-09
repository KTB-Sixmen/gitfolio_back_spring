package com.be.gitfolio.payment.infrastructure;

import com.be.gitfolio.common.type.PaidPlan;
import com.be.gitfolio.payment.domain.Payment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
public interface PaymentRepository extends JpaRepository<Payment, Long> {
    Optional<Payment> findByTransactionId(String transactionId);
    Optional<Payment> findByMemberIdAndPaidPlan(Long memberId, PaidPlan paidPlan);

    Optional<Payment> findByMemberId(Long memberId);
}
