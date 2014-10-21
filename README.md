# VMX Middle: REST API
<img src="vmx_single.png" style="width: 300px"></img>

***Copyright 2013-2014 vision.ai, LLC. All rights reserved.***

VMX Middle runs on top of VMX server and serves the VMX App Builder
over HTTP.  This document describes the REST API provided by VMX.

1. [The VMX REST API](#API)
2. [Online help](#help)

#### Related Pages
If you are interested in learning about main VMX application, see
[VMX Docs](VMX.html).

If you want to learn about interactively training models in the
browser, see [VMX App Builder Docs](VMXAppBuilder).

Visit us on the web at http://vision.ai

### <a name="API"></a> 1. The VMX REST API


Route        | Description
-------------| ----------
GET /session  | List open sessions
GET /model   |  List available models
POST /session |  Create a new session
POST /session/#session_id   |  Detect objects inside the image
GET /session/#session_id/params  | List loaded model's parameters
POST /session/#session_id/edit   |  Edit the model
POST /session/#session_id/load   |  Load a model
GET /check  | Check VMX version and whether this copy is licensed
POST /activate/#key | Active this copy of VMX
GET /random | Return a random image from the models

### <a name="help"></a> 2. Online help

Stuck? Visit the VMX forums at https://forums.vision.ai for more information,
tutorial, FAQs, and discussions regarding common installation issues.

<img src="v_square.png" style="width: 30px"></img>
