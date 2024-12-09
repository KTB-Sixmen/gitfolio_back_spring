package com.be.gitfolio.resume.domain;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

public class LikeTest {

    @Test
    void 이력서_ID와_사용자_ID로_좋아요를_생성할_수_있다() throws Exception {
        //given

        //when
        Like like = Like.of("testResumeId", 1L);
        //then
        Assertions.assertThat(like.getMemberId()).isEqualTo(1L);
    }
}
