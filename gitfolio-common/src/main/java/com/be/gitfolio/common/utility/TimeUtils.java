package com.be.gitfolio.common.utility;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.ZonedDateTime;

public class TimeUtils {

    private static final ZoneId UTC_ZONE = ZoneId.of("UTC");
    private static final ZoneId SEOUL_ZONE = ZoneId.of("Asia/Seoul");

    /**
     * UTC LocalDateTime을 서울(LocalDateTime)로 변환합니다.
     *
     * @param utcTime UTC 기준 LocalDateTime
     * @return 서울 기준 LocalDateTime
     */
    public static LocalDateTime convertUtcToSeoul(LocalDateTime utcTime) {
        return ZonedDateTime.of(utcTime, UTC_ZONE)
                .withZoneSameInstant(SEOUL_ZONE)
                .toLocalDateTime();
    }
}
