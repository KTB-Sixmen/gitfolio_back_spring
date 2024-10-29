package com.be.gitfolio.resume.mapper;

import com.be.gitfolio.common.grpc.MemberServiceProto;
import com.be.gitfolio.common.type.EmploymentStatus;
import com.be.gitfolio.common.type.GraduationStatus;
import com.be.gitfolio.common.type.SchoolType;
import com.be.gitfolio.common.type.WorkType;
import com.be.gitfolio.resume.domain.Resume.*;

public class MemberMapper {

    public static Certificate toEntity(MemberServiceProto.Certificate certificateProto) {
        return Certificate.builder()
                .certificateName(certificateProto.getCertificateName())
                .certificateGrade(certificateProto.getCertificateGrade())
                .certificatedAt(certificateProto.getCertificatedAt())
                .certificateOrganization(certificateProto.getCertificateOrganization())
                .build();
    }

    public static Education toEntity(MemberServiceProto.Education educationProto) {
        return Education.builder()
                .schoolType(SchoolType.fromString(educationProto.getSchoolType()))
                .schoolName(educationProto.getSchoolName())
                .major(educationProto.getMajor())
                .graduationStatus(GraduationStatus.fromString(educationProto.getGraduationStatus()))
                .startedAt(educationProto.getStartedAt())
                .endedAt(educationProto.getEndedAt())
                .build();
    }

    public static Link toEntity(MemberServiceProto.Link linkProto) {
        return Link.builder()
                .linkTitle(linkProto.getLinkTitle())
                .linkUrl(linkProto.getLinkUrl())
                .build();
    }


    public static WorkExperience toEntity(MemberServiceProto.WorkExperience workExperienceProto) {
        return WorkExperience.builder()
                .companyName(workExperienceProto.getCompanyName())
                .departmentName(workExperienceProto.getDepartmentName())
                .role(workExperienceProto.getRole())
                .workType(WorkType.fromString(workExperienceProto.getWorkType()))
                .employmentStatus(EmploymentStatus.fromString(workExperienceProto.getEmploymentStatus()))
                .startedAt(workExperienceProto.getStartedAt())
                .endedAt(workExperienceProto.getEndedAt())
                .build();
    }

}
