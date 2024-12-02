package com.be.gitfolio.member.domain;

import lombok.Builder;
import lombok.Getter;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.List;

import static com.be.gitfolio.member.domain.request.MemberAdditionalInfoRequest.*;

@Builder
@Getter
public class MemberAdditionalInfo {

    private String id;
    private String memberId;
    private List<WorkExperience> workExperiences;
    private List<Education> educations;
    private List<Certificate> certificates;
    private List<Link> links;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    public static MemberAdditionalInfo from(Long memberId) {
        return MemberAdditionalInfo.builder()
                .memberId(String.valueOf(memberId))
                .workExperiences(Collections.emptyList())
                .educations(Collections.emptyList())
                .certificates(Collections.emptyList())
                .links(Collections.emptyList())
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();
    }

    public MemberAdditionalInfo updateMemberAdditionalInfo(MemberAdditionalInfoUpdate memberAdditionalInfoUpdate) {
        return MemberAdditionalInfo.builder()
                .id(id)
                .memberId(memberId)
                .workExperiences(memberAdditionalInfoUpdate.workExperiences())
                .educations(memberAdditionalInfoUpdate.educations())
                .certificates(memberAdditionalInfoUpdate.certificates())
                .links(memberAdditionalInfoUpdate.links())
                .createdAt(createdAt)
                .updatedAt(LocalDateTime.now())
                .build();
    }
}
