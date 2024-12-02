package com.be.gitfolio.member.domain;

import com.be.gitfolio.common.type.EmploymentStatus;
import com.be.gitfolio.common.type.GraduationStatus;
import com.be.gitfolio.common.type.SchoolType;
import com.be.gitfolio.common.type.WorkType;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static com.be.gitfolio.member.domain.request.MemberAdditionalInfoRequest.*;
import static org.assertj.core.api.Assertions.*;

public class MemberAdditionalInfoTest {

    @Test
    void MemberAdditionalInfo는_초기에_memberId_값만_넣고_나머지는_빈_리스트로_초기화한다() throws Exception {
        //given
        Long memberId = 1L;
        //when
        MemberAdditionalInfo memberAdditionalInfo = MemberAdditionalInfo.from(memberId);
        //then
        assertThat(memberAdditionalInfo.getMemberId()).isEqualTo(String.valueOf(memberId));
        assertThat(memberAdditionalInfo.getWorkExperiences()).isEmpty();
        assertThat(memberAdditionalInfo.getEducations()).isEmpty();
        assertThat(memberAdditionalInfo.getCertificates()).isEmpty();
        assertThat(memberAdditionalInfo.getLinks()).isEmpty();
    }

    @Test
    void MemberAdditionalInfo는_MemberAdditionalInfoUpdate로_수정할_수_있다() throws Exception {
        //given
        Long memberId = 1L;
        MemberAdditionalInfo memberAdditionalInfo = MemberAdditionalInfo.from(memberId);

        List<WorkExperience> workExperienceList = new ArrayList<>();
        WorkExperience workExperience = WorkExperience.builder()
                .role("workRole")
                .companyName("workCompanyName")
                .departmentName("workDepartmentName")
                .employmentStatus(EmploymentStatus.RESIGNATION)
                .startedAt("2000-11-11")
                .endedAt("2000-11-13")
                .workType(WorkType.FREELANCER)
                .build();
        workExperienceList.add(workExperience);

        List<Education> educationList = new ArrayList<>();
        Education education = Education.builder()
                .schoolType(SchoolType.HIGH_SCHOOL)
                .major("educationMajor")
                .schoolName("educationSchoolName")
                .graduationStatus(GraduationStatus.GRADUATED)
                .startedAt("2000-11-11")
                .endedAt("2000-11-13")
                .build();
        educationList.add(education);

        List<Certificate> certificateList = new ArrayList<>();
        Certificate certificate = Certificate.builder()
                .certificateOrganization("certificateOrganization")
                .certificateName("certificationName")
                .certificateGrade("certificateGrade")
                .certificatedAt("2000-11-11")
                .build();
        certificateList.add(certificate);

        List<Link> linkList = new ArrayList<>();
        Link link = Link.builder()
                .linkTitle("linkTitle")
                .linkUrl("linkUrl")
                .build();
        linkList.add(link);

        MemberAdditionalInfoUpdate memberAdditionalInfoUpdate = MemberAdditionalInfoUpdate.builder()
                .workExperiences(workExperienceList)
                .educations(educationList)
                .certificates(certificateList)
                .links(linkList)
                .build();

        //when
        MemberAdditionalInfo updatedMemberAdditionalInfo = memberAdditionalInfo.updateMemberAdditionalInfo(memberAdditionalInfoUpdate);

        //then
        assertThat(updatedMemberAdditionalInfo.getWorkExperiences()).isEqualTo(workExperienceList);
        assertThat(updatedMemberAdditionalInfo.getEducations()).isEqualTo(educationList);
        assertThat(updatedMemberAdditionalInfo.getCertificates()).isEqualTo(certificateList);
        assertThat(updatedMemberAdditionalInfo.getLinks()).isEqualTo(linkList);
    }

}
