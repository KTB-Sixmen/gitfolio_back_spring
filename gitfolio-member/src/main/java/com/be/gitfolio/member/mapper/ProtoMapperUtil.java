package com.be.gitfolio.member.mapper;

import com.be.gitfolio.common.grpc.MemberServiceProto;
import com.be.gitfolio.member.domain.Member;
import com.be.gitfolio.member.domain.MemberAdditionalInfo;

import java.util.Optional;

public class ProtoMapperUtil {

    public static MemberServiceProto.MemberResponseById toProto(Member member, MemberAdditionalInfo additionalInfo) {
        MemberServiceProto.MemberResponseById.Builder builder = MemberServiceProto.MemberResponseById.newBuilder();

        // Member 필드 매핑
        if (member != null) {
            builder.setMemberId(String.valueOf(member.getId()))
                    .setNickname(member.getNickname())
                    .setMemberName(member.getName())
                    .setGithubName(Optional.ofNullable(member.getGithubName()).orElse(""))
                    .setAvatarUrl(member.getAvatarUrl())
                    .setPhoneNumber(member.getPhoneNumber())
                    .setEmail(member.getEmail());
            if (member.getPosition() != null) {
                builder.setPosition(member.getPosition().name());
            }
        }

        // MemberAdditionalInfo 필드 매핑
        if (additionalInfo != null) {
            builder.addAllWorkExperiences(
                    additionalInfo.getWorkExperiences().stream()
                            .map(ProtoMapperUtil::toProto)
                            .toList()
            );
            builder.addAllEducations(
                    additionalInfo.getEducations().stream()
                            .map(ProtoMapperUtil::toProto)
                            .toList()
            );
            builder.addAllCertificates(
                    additionalInfo.getCertificates().stream()
                            .map(ProtoMapperUtil::toProto)
                            .toList()
            );
            builder.addAllLinks(
                    additionalInfo.getLinks().stream()
                            .map(ProtoMapperUtil::toProto)
                            .toList()
            );
        }

        return builder.build();
    }

    public static MemberServiceProto.WorkExperience toProto(MemberAdditionalInfo.WorkExperience workExperience) {
        return MemberServiceProto.WorkExperience.newBuilder()
                .setCompanyName(workExperience.getCompanyName())
                .setDepartmentName(workExperience.getDepartmentName())
                .setRole(workExperience.getRole())
                .setWorkType(workExperience.getWorkType().name())
                .setEmploymentStatus(workExperience.getEmploymentStatus().name())
                .setStartedAt(workExperience.getStartedAt())
                .setEndedAt(workExperience.getEndedAt())
                .build();
    }

    public static MemberServiceProto.Education toProto(MemberAdditionalInfo.Education education) {
        return MemberServiceProto.Education.newBuilder()
                .setSchoolType(education.getSchoolType().name())
                .setSchoolName(education.getSchoolName())
                .setMajor(education.getMajor())
                .setGraduationStatus(education.getGraduationStatus().name())
                .setStartedAt(education.getStartedAt())
                .setEndedAt(education.getEndedAt())
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
}