package com.be.gitfolio.common.s3;

import com.amazonaws.services.s3.AmazonS3Client;
import com.amazonaws.services.s3.model.ObjectMetadata;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class S3Service {

    private final AmazonS3Client amazonS3Client;

    @Value("${cloud.aws.s3.bucket}")
    private String bucket;
    @Value("${cloud.aws.s3.url-prefix}")
    private String s3UrlPrefix;

    /**
     * 이미지 파일 업로드
     */
    public String uploadFile(MultipartFile file) throws IOException {
        // 원본 파일명
        String originalFileName = file.getOriginalFilename();
        // 저장 파일명
        String storeFileName = createStoreFileName(originalFileName);

        log.info("storeFileName : {}", storeFileName);


        // S3에 저장
        ObjectMetadata metadata = new ObjectMetadata();
        metadata.setContentType(file.getContentType());
        metadata.setContentLength(file.getSize());
        amazonS3Client.putObject(bucket, storeFileName, file.getInputStream(), metadata);

        return storeFileName;
    }

    /**
     * S3 버킷에서 파일 삭제
     */
    public void deleteFile(String storeFileName) {
        amazonS3Client.deleteObject(bucket, storeFileName);
    }

    /**
     * 파일명이 겹치는 것을 방지하기위해 중복되지않는 UUID를 생성해서 반환(ext는 확장자)
     */
    private String createStoreFileName(String originalFilename) {
        String ext = extractExt(originalFilename);
        String uuid = UUID.randomUUID().toString();
        return uuid + "." + ext;
    }

    /**
     * 파일 확장자를 추출하기 위해 만든 메서드
     */
    private String extractExt(String originalFilename) {
        int post = originalFilename.lastIndexOf(".");
        return originalFilename.substring(post + 1);
    }

    /**
     * S3 URL prefix 포함한 경로 생성 메서드
     */
    public String getFullFileUrl(String filename) {
        return s3UrlPrefix + filename;
    }
}

