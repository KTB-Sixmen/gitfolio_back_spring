package com.be.gitfolio.chat.repository;

import com.be.gitfolio.chat.domain.MemberSession;
import org.springframework.data.mongodb.repository.ReactiveMongoRepository;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Mono;

@Repository
public interface MemberSessionRepository extends ReactiveMongoRepository<MemberSession, String> {
    Mono<MemberSession> findByMemberId(Long memberId);
}
