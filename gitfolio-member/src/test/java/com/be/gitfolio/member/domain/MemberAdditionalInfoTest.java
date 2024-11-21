package com.be.gitfolio.member.domain;

import com.be.gitfolio.common.type.WorkType;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.be.gitfolio.member.domain.MemberAdditionalInfoRequest.*;
import static org.assertj.core.api.Assertions.*;

public class MemberAdditionalInfoTest {

    @Test
    void MemberAdditionalInfo는_초기에_memberId_값만_넣고_나머지는_빈_리스트로_초기화한다() throws Exception {
        //given
        Long memberId = 1L;
        //when
        MemberAdditionalInfo memberAdditionalInfo = MemberAdditionalInfo.from(memberId);
        //then
        assertThat(memberAdditionalInfo.getMemberId()).isEqualTo(memberId);
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
                .workType(WorkType.FREELANCER)
                .build();
        workExperienceList.add(workExperience);

        MemberAdditionalInfoUpdate memberAdditionalInfoUpdate = MemberAdditionalInfoUpdate.builder()
                .workExperiences(workExperienceList)
                .educations(Collections.emptyList())
                .certificates(Collections.emptyList())
                .links(Collections.emptyList())
                .build();

        //when
        memberAdditionalInfo.updateMemberAdditionalInfo(memberAdditionalInfoUpdate);
        //then
        assertThat(memberAdditionalInfo.getWorkExperiences()).isEqualTo(workExperienceList);
        assertThat(memberAdditionalInfo.getEducations()).isEmpty();
        assertThat(memberAdditionalInfo.getCertificates()).isEmpty();
        assertThat(memberAdditionalInfo.getLinks()).isEmpty();
    }

}
