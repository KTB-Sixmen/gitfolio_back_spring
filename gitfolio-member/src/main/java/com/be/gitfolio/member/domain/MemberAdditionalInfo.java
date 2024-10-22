package com.be.gitfolio.member.domain;

import com.be.gitfolio.common.config.BaseEntityMongo;
import com.be.gitfolio.member.dto.MemberRequestDTO;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import java.util.Collections;
import java.util.List;

import static com.be.gitfolio.member.dto.MemberRequestDTO.*;

@Getter
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Document(collection = "member_additional_info")
public class MemberAdditionalInfo extends BaseEntityMongo {

    @Id
    private String id;
    private String memberId; // 회원 ID
    private List<WorkExperience> workExperiences;
    private List<Education> educations;
    private List<Certificate> certificates;
    private List<Activity> activities;
    private List<Link> links;

    public static MemberAdditionalInfo from(Long memberId) {
        return MemberAdditionalInfo.builder()
                .memberId(memberId.toString())
                .workExperiences(Collections.emptyList())
                .educations(Collections.emptyList())
                .certificates(Collections.emptyList())
                .activities(Collections.emptyList())
                .links(Collections.emptyList())
                .build();
    }
    public static MemberAdditionalInfo of(Long memberId, MemberAdditionalRequestDTO memberAdditionalRequestDTO) {
        return MemberAdditionalInfo.builder()
                .memberId(memberId.toString())
                .workExperiences(memberAdditionalRequestDTO.getWorkExperiences())
                .educations(memberAdditionalRequestDTO.getEducations())
                .certificates(memberAdditionalRequestDTO.getCertificates())
                .activities(memberAdditionalRequestDTO.getActivities())
                .links(memberAdditionalRequestDTO.getLinks())
                .build();
    }


    public void updateMemberAdditionalInfo(MemberAdditionalRequestDTO memberAdditionalRequestDTO) {
        this.workExperiences = memberAdditionalRequestDTO.getWorkExperiences();
        this.educations = memberAdditionalRequestDTO.getEducations();
        this.certificates = memberAdditionalRequestDTO.getCertificates();
        this.activities = memberAdditionalRequestDTO.getActivities();
        this.links = memberAdditionalRequestDTO.getLinks();
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class WorkExperience {
        private String companyName;
        private String departmentName;
        private String role;
        private String workTime;
        private String employmentStatus;
        private String startedAt;
        private String endedAt;
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Education {
        private String schoolType;
        private String schoolName;
        private String major;
        private String graduationStatus;
        private String startAt;
        private String endedAt;
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Certificate {
        private String certificateName;
        private String certificateGrade;
        private String certificatedAt;
        private String certificateOrganization;
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Activity {
        private String activityName;
        private int activityYear;
        private String activityDescription;
        private String activityOrganization;
    }

    @Getter
    @Builder
    @AllArgsConstructor
    @NoArgsConstructor
    public static class Link {
        private String linkTitle;
        private String linkUrl;
    }
}
