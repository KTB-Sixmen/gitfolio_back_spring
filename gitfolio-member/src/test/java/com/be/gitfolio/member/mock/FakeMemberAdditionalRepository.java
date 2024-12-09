package com.be.gitfolio.member.mock;

import com.be.gitfolio.member.domain.MemberAdditionalInfo;
import com.be.gitfolio.member.service.port.MemberAdditionalInfoRepository;

import java.util.*;

public class FakeMemberAdditionalRepository implements MemberAdditionalInfoRepository {

    private final List<MemberAdditionalInfo> data = new ArrayList<>();

    @Override
    public MemberAdditionalInfo save(MemberAdditionalInfo memberAdditionalInfo) {
        if (memberAdditionalInfo.getId() == null) {
            MemberAdditionalInfo newMemberAdditionalInfo = MemberAdditionalInfo.builder()
                    .id(UUID.randomUUID().toString())
                    .memberId(memberAdditionalInfo.getMemberId())
                    .workExperiences(memberAdditionalInfo.getWorkExperiences())
                    .educations(memberAdditionalInfo.getEducations())
                    .certificates(memberAdditionalInfo.getCertificates())
                    .links(memberAdditionalInfo.getLinks())
                    .build();
            data.add(newMemberAdditionalInfo);
            return newMemberAdditionalInfo;
        } else {
            data.removeIf(item -> Objects.equals(item.getId(), memberAdditionalInfo.getId()));
            data.add(memberAdditionalInfo);
            return memberAdditionalInfo;
        }
    }

    @Override
    public Optional<MemberAdditionalInfo> findByMemberId(Long memberId) {
        return data.stream().filter(item -> item.getMemberId().equals(memberId)).findAny();
    }

    @Override
    public void deleteByMemberId(Long memberId) {
        data.removeIf(item -> item.getMemberId().equals(memberId));
    }
}
