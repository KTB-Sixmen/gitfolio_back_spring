package com.be.gitfolio.member.mapper;

import com.be.gitfolio.common.grpc.MemberServiceProto;
import com.be.gitfolio.member.domain.MemberAdditionalInfo;

public class ProtoMapper {

    public static MemberServiceProto.Activity toProto(MemberAdditionalInfo.Activity activity) {
        return MemberServiceProto.Activity.newBuilder()
                .setActivityName(activity.getActivityName())
                .setActivityYear(activity.getActivityYear())
                .setActivityDescription(activity.getActivityDescription())
                .setActivityOrganization(activity.getActivityOrganization())
                .build();
    }

    public static MemberServiceProto.Certificate toProto(MemberAdditionalInfo.Certificate certificate) {
        return MemberServiceProto.Certificate.newBuilder()
                .setCertificateName(certificate.getCertificateName())
                .setCertificateGrade(certificate.getCertificateGrade())
                .setCertificatedAt(certificate.getCertificatedAt())
                .setCertificateOrganization(certificate.getCertificateOrganization())
                .build();
    }

    public static MemberServiceProto.Link toProto(MemberAdditionalInfo.Link link) {
        return MemberServiceProto.Link.newBuilder()
                .setLinkTitle(link.getLinkTitle())
                .setLinkUrl(link.getLinkUrl())
                .build();
    }

    public static MemberServiceProto.Education toProto(MemberAdditionalInfo.Education education) {
        return MemberServiceProto.Education.newBuilder()
                .setSchoolType(String.valueOf(education.getSchoolType()))
                .setSchoolName(education.getSchoolName())
                .setMajor(education.getMajor())
                .setGraduationStatus(String.valueOf(education.getGraduationStatus()))
                .setStartedAt(education.getStartedAt())
                .setEndedAt(education.getEndedAt())
                .build();
    }

    public static MemberServiceProto.WorkExperience toProto(MemberAdditionalInfo.WorkExperience workExperience) {
        return MemberServiceProto.WorkExperience.newBuilder()
                .setCompanyName(workExperience.getCompanyName())
                .setDepartmentName(workExperience.getDepartmentName())
                .setRole(workExperience.getRole())
                .setWorkType(String.valueOf(workExperience.getWorkType()))
                .setEmploymentStatus(String.valueOf(workExperience.getEmploymentStatus()))
                .setStartedAt(workExperience.getStartedAt())
                .setEndedAt(workExperience.getEndedAt())
                .build();
    }
}
