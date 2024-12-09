package com.be.gitfolio.member.infrastructure.memberAdditionalInfo;

import com.be.gitfolio.common.dto.Certificate;
import com.be.gitfolio.common.dto.Education;
import com.be.gitfolio.common.dto.Link;
import com.be.gitfolio.common.dto.WorkExperience;
import com.be.gitfolio.member.domain.*;
import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor(access = AccessLevel.PROTECTED)
@Document(collection = "member_additional_info")
public class MemberAdditionalInfoEntity {

    @Id
    private String id;
    private Long memberId; // 회원 ID
    private List<WorkExperience> workExperiences;
    private List<Education> educations;
    private List<Certificate> certificates;
    private List<Link> links;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    public static MemberAdditionalInfoEntity fromModel(MemberAdditionalInfo memberAdditionalInfo) {
        return MemberAdditionalInfoEntity.builder()
                .id(memberAdditionalInfo.getId())
                .memberId(memberAdditionalInfo.getMemberId())
                .memberId(memberAdditionalInfo.getMemberId())
                .workExperiences(memberAdditionalInfo.getWorkExperiences())
                .educations(memberAdditionalInfo.getEducations())
                .certificates(memberAdditionalInfo.getCertificates())
                .links(memberAdditionalInfo.getLinks())
                .createdAt(memberAdditionalInfo.getCreatedAt())
                .updatedAt(memberAdditionalInfo.getUpdatedAt())
                .build();
    }

    public MemberAdditionalInfo toModel() {
        return MemberAdditionalInfo.builder()
                .id(id)
                .memberId(memberId)
                .workExperiences(workExperiences)
                .educations(educations)
                .certificates(certificates)
                .links(links)
                .createdAt(createdAt)
                .updatedAt(updatedAt)
                .build();
    }
}
