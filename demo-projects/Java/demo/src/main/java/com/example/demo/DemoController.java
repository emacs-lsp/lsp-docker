package com.example.demo;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * DemoController
 */
@Controller
public class DemoController {
  @GetMapping(value = "/")
  public User helloBoot(@RequestParam(required = false) String name) {
    User result = new User();
    result.setName(name);
    return result;
  }
}
