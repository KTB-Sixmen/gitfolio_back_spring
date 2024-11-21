package com.be.gitfolio.member.service.port;

import com.be.gitfolio.member.domain.Member;

import java.util.Optional;

public interface MemberRepository {
    Member save(Member member);

    Optional<Member> findByUsername(String username);

    Optional<Member> findById(Long memberId);

    void deleteById(Long memberId);
}
