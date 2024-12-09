package com.be.gitfolio.member.domain;

import com.be.gitfolio.common.dto.Certificate;
import com.be.gitfolio.common.dto.Education;
import com.be.gitfolio.common.dto.Link;
import com.be.gitfolio.common.dto.WorkExperience;
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
    private Long memberId;
    private List<WorkExperience> workExperiences;
    private List<Education> educations;
    private List<Certificate> certificates;
    private List<Link> links;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    public static MemberAdditionalInfo from(Long memberId) {
        return MemberAdditionalInfo.builder()
                .memberId(memberId)
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
