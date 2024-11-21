package com.be.gitfolio.member.infrastructure.member;

import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.service.port.MemberRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.Optional;

@Repository
@RequiredArgsConstructor
public class MemberRepositoryImpl implements MemberRepository {

    private final MemberJpaRepository memberJpaRepository;

    @Override
    public Member save(Member member) {
        return memberJpaRepository.save(MemberEntity.fromModel(member)).toModel();
    }

    @Override
    public Optional<Member> findByUsername(String username) {
        return memberJpaRepository.findByUsername(username).map(MemberEntity::toModel);
    }

    @Override
    public Optional<Member> findById(Long memberId) {
        return memberJpaRepository.findById(memberId).map(MemberEntity::toModel);
    }

    @Override
    public void deleteById(Long memberId) {
        memberJpaRepository.deleteById(memberId);
    }
}
