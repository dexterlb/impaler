- Get the interpreter working as described in the module-system note
    - Use a transparent closure object instead of opaque functions wherever possible
        - This will make code debuggable
    - cur-env and get-env: probably make get-env take a closure as argument and return
      its argument, and then cur-env can be a macro that uses get-env
- Outline the module system paper
