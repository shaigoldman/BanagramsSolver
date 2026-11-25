package com.shaigoldman.bananaboard;

import static org.junit.Assert.*;
import android.webkit.WebView;
import androidx.test.core.app.ActivityScenario;
import androidx.test.ext.junit.runners.AndroidJUnit4;
import org.junit.Test;
import org.junit.runner.RunWith;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

@RunWith(AndroidJUnit4.class)
public class OfflineSolverTest {
    private String evaluate(ActivityScenario<MainActivity> scenario, String code) throws Exception {
        CountDownLatch latch = new CountDownLatch(1);
        AtomicReference<String> result = new AtomicReference<>();
        scenario.onActivity(activity -> ((WebView) activity.findViewById(android.R.id.content).getRootView()
                .findViewWithTag("solver-webview")).evaluateJavascript(code, value -> {
                    result.set(value);
                    latch.countDown();
                }));
        assertTrue("JavaScript timed out", latch.await(10, TimeUnit.SECONDS));
        return result.get();
    }
    private void waitFor(ActivityScenario<MainActivity> scenario, String expression, int seconds) throws Exception {
        long end = System.currentTimeMillis() + seconds * 1000L;
        while (System.currentTimeMillis() < end) {
            if ("true".equals(evaluate(scenario, "Boolean(" + expression + ")"))) return;
            Thread.sleep(500);
        }
        fail("Timed out waiting for " + expression + "; page: " + evaluate(scenario, "document.body.innerText"));
    }
    private void solve(ActivityScenario<MainActivity> scenario, String letters) throws Exception {
        waitFor(scenario, "document.querySelector('#letters')", 30);
        evaluate(scenario, "(()=>{let e=document.querySelector('#letters'); Object.getOwnPropertyDescriptor(HTMLTextAreaElement.prototype,'value').set.call(e,'" + letters + "');e.dispatchEvent(new Event('input',{bubbles:true}));})()");
        waitFor(scenario, "document.querySelector('button.primary:not([disabled])')", 10);
        evaluate(scenario, "document.querySelector('button.primary').click()");
        waitFor(scenario, "document.body.innerText.includes('All together now.')", 150);
        assertTrue(evaluate(scenario, "document.body.innerText").contains("Words in your board"));
    }
    @Test public void solvesShortAndLongHandsOffline() throws Exception {
        try (ActivityScenario<MainActivity> scenario = ActivityScenario.launch(MainActivity.class)) {
            solve(scenario, "CAT");
            evaluate(scenario, "document.querySelector('button.again').click()");
            solve(scenario, "REPRESENTATION");
        }
    }
}
