package com.be.gitfolio.resume.domain;

import lombok.RequiredArgsConstructor;
import org.junit.jupiter.api.Test;


import static org.assertj.core.api.Assertions.*;

@RequiredArgsConstructor
public class CommentTest {

    @Test
    void updateContent로_댓글을_수정할_수_있다() throws Exception {
        //given
        Comment comment = Comment.builder()
                .id(1L)
                .resumeId("testResumeId")
                .memberId(1L)
                .content("test123")
                .build();
        //when
        comment.updateContent("update123");
        //then
        assertThat(comment.getContent()).isEqualTo("update123");
    }
}
