package com.be.gitfolio.resume.domain;

import com.be.gitfolio.common.dto.*;
import com.be.gitfolio.common.exception.BaseException;
import com.be.gitfolio.common.type.*;
import com.be.gitfolio.resume.dto.ResumeRequestDTO;
import com.be.gitfolio.resume.dto.ResumeResponseDTO;
import com.be.gitfolio.resume.type.Template;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static com.be.gitfolio.resume.domain.Resume.*;
import static com.be.gitfolio.resume.dto.ResumeRequestDTO.*;
import static com.be.gitfolio.resume.dto.ResumeResponseDTO.*;
import static org.assertj.core.api.Assertions.*;

public class ResumeTest {

    @Test
    void MemberInfoDTO와_AIResponseDTO와_CreateResumeRequestDTO로_이력서를_생성할_수_있다() throws Exception {
        //given
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

        MemberInfoDTO memberInfoDTO = new MemberInfoDTO(1L,
                "testId1",
                "nickname",
                "name",
                "username",
                "githubName",
                "avatarUrl",
                "01011111111",
                "a1234@gmail.com",
                PositionType.BACKEND,
                PaidPlan.FREE,
                2,
                workExperienceList,
                educationList,
                certificateList,
                linkList);

        List<Project> projects = new ArrayList<>();
        Project project = Project.builder()
                .projectEndedAt("2000-00-00")
                .projectStartedAt("1999-00-00")
                .projectName("projectName")
                .repoLink("www.repoLink.com")
                .skillSet("Java")
                .projectDescription("projectDescription")
                .build();
        projects.add(project);

        List<String> techStacks = new ArrayList<>();
        techStacks.add("Java");
        AIResponseDTO aiResponseDTO = new AIResponseDTO(projects, techStacks, "aboutMe");

        List<String> selectedRepo = new ArrayList<>();
        selectedRepo.add("repo1");

        CreateResumeRequestDTO createResumeRequestDTO = new CreateResumeRequestDTO(selectedRepo, "requirements", Visibility.PUBLIC, Template.GITFOLIO);
        //when
        Resume resume = of(memberInfoDTO, aiResponseDTO, createResumeRequestDTO);
        //then
        assertThat(resume.getAboutMe()).isEqualTo("aboutMe");
        assertThat(resume.getEducations().get(0).getEndedAt()).isEqualTo("2000-11-13");
    }

    @Test
    void UpdateResumeRequestDTO와_avatarUrl로_이력서를_수정할_수_있다() throws Exception {
        //given
        List<Link> linkList = new ArrayList<>();
        Link link = Link.builder()
                .linkTitle("linkTitle")
                .linkUrl("linkUrl")
                .build();
        linkList.add(link);

        Resume resume = builder()
                .avatarUrl("avatarUrl")
                .build();

        List<String> techStacks = new ArrayList<>();
        techStacks.add("updateTechStacks");

        List<String> tags = new ArrayList<>();
        tags.add("updateTag");

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

        List<Project> projects = new ArrayList<>();
        Project project = Project.builder()
                .projectEndedAt("2000-00-00")
                .projectStartedAt("1999-00-00")
                .projectName("projectName")
                .repoLink("www.repoLink.com")
                .skillSet("Java")
                .projectDescription("projectDescription")
                .build();
        projects.add(project);

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

        UpdateResumeRequestDTO updateResumeRequestDTO = new UpdateResumeRequestDTO(techStacks, "updateAboutMe", tags, workExperienceList, projects, linkList, educationList, certificateList);
        String updateAvatarUrl = "updateUrl";
        //when
        resume.updateResume(updateResumeRequestDTO, updateAvatarUrl);
        //then
        assertThat(resume.getAvatarUrl()).isEqualTo("updateUrl");
        assertThat(resume.getAboutMe()).isEqualTo("updateAboutMe");
    }

    @Test
    void increaseView로_조회수를_증가시킬_수_있다() throws Exception {
        //given
        Resume resume = builder()
                .viewCount(0)
                .build();
        //when
        resume.increaseView();
        //then
        assertThat(resume.getViewCount()).isEqualTo(1);
    }

    @Test
    void updateVisibility로_공개_여부를_변경할_수_있다() throws Exception {
        //given
        Resume resume = builder()
                .visibility(Visibility.PUBLIC)
                .build();
        //when
        resume.updateVisibility(Visibility.PRIVATE);
        //then
        assertThat(resume.getVisibility()).isEqualTo(Visibility.PRIVATE);
    }

    @Test
    void validateVisibility는_PRIVATE_이력서이면_예외를_던진다() throws Exception {
        //given
        Resume resume = builder()
                .visibility(Visibility.PRIVATE)
                .build();

        //when
        //then
        assertThatThrownBy(resume::validateVisibility).isInstanceOf(BaseException.class);

    }

    @Test
    void validateOwnerShip은_이력서의_사용자ID와_접근하려는_사용자ID가_다르면_예외를_던진다() throws Exception {
        //given
        Resume resume = builder()
                .memberId(2L)
                .build();

        //when
        //then
        assertThatThrownBy(() -> {
            resume.validateOwnerShip(1L);
        }).isInstanceOf(BaseException.class);

    }
}
